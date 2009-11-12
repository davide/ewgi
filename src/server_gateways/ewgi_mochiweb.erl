%%%-------------------------------------------------------------------
%%% File    : ewgi_mochiweb.erl
%%% Authors : Filippo Pacini <filippo.pacini@gmail.com>
%%%           Hunter Morris <huntermorris@gmail.com>
%%% License :
%%% The contents of this file are subject to the Mozilla Public
%%% License Version 1.1 (the "License"); you may not use this file
%%% except in compliance with the License. You may obtain a copy of
%%% the License at http://www.mozilla.org/MPL/
%%%
%%% Software distributed under the License is distributed on an "AS IS"
%%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%%% the License for the specific language governing rights and
%%% limitations under the License.
%%% The Initial Developer of the Original Code is S.G. Consulting
%%% srl. Portions created by S.G. Consulting s.r.l. are Copyright (C)
%%% 2007 S.G. Consulting srl. All Rights Reserved.
%%%
%%% @doc 
%%% <p>Reference implementation of a MochiWeb EWGI server gateway.</p>
%%%
%%% @end
%%%
%%% Created : 12 Oct 2007 by Filippo Pacini <filippo.pacini@gmail.com>
%%%-------------------------------------------------------------------
-module(ewgi_mochiweb).

%% ewgi callbacks
-export([run/2]).
-export([
		stream_process_deliver/2,
		stream_process_deliver_chunk/2,
		stream_process_deliver_final_chunk/2,
		stream_process_end/2
	]).

-include_lib("ewgi.hrl").

-define(EWGI2MOCHI(Err, Hdrs), {element(1, Err), Hdrs, element(2, Err)}).

%%====================================================================
%% ewgi_server callbacks
%%====================================================================
run(Appl, MochiReq) ->
    try parse_arg(MochiReq) of
        Req when ?IS_EWGI_REQUEST(Req) ->
            try process_application(Appl, ewgi_api:context(Req, ewgi_api:empty_response())) of
                not_found ->
                    MochiReq:not_found();
                Ctx when ?IS_EWGI_CONTEXT(Ctx) ->
                    handle_result(?INSPECT_EWGI_RESPONSE(Ctx), MochiReq)
            catch
                _:Reason ->
                    error_logger:error_report(io_lib:format("Responding with 500 INTERNAL SERVER ERROR.~nReason: ~p~nStack: ~p~n", [Reason, erlang:get_stacktrace()])),
                    MochiReq:respond({500, [], "Internal server error"})
            end
    catch
        _:Reason ->
            error_logger:error_report(io_lib:format("Responding with 400 BAD REQUEST.~nReason: ~p~nStack: ~p~n", [Reason, erlang:get_stacktrace()])),
            MochiReq:respond({400, [], "Bad request"})
    end.


%% Chunked response if a nullary function is returned
handle_result(Ctx, Req) ->
	case ewgi_api:response_message_body(Ctx) of
		{push_stream, GeneratorPid, Timeout} when is_pid(GeneratorPid) ->
			handle_push_stream(Ctx, Req, GeneratorPid, Timeout);
		Body ->
			{Code, _} = ewgi_api:response_status(Ctx),
			Headers = ewgi_api:response_headers(Ctx),
			handle_result1(Code, Headers, Body, Req)
	end.

handle_result1(Code, Headers, F, Req) when is_function(F, 0) ->
    MochiResp = Req:respond({Code, Headers, chunked}),
    %handle_stream_result(MochiResp, (catch F()));
    handle_stream(MochiResp, F);
handle_result1(Code, Headers, L, Req) ->
    Req:respond({Code, Headers, L}).

handle_push_stream(Ctx, Req, GeneratorPid, Timeout) ->
	Socket = Req:get(socket),
	GeneratorPid ! {push_stream_init, ?MODULE, self(), Socket},
	receive
	{push_stream_init, GeneratorPid, Code, Headers, TransferEncoding} ->
		case TransferEncoding of
			chunked ->
				Req:respond({Code, Headers, chunked}),
				wait_for_streamcontent_pid(Socket, GeneratorPid)
			;_ ->
				%% mochiweb_request:respond/1 expects the full body in order to
				%% count the content-length but we already have that. What we're
				%% missing is the [to-be-sent] body.
				HResponse = mochiweb_headers:make(Headers),
				Req:start_response({Code, HResponse}),
				%% WARNING: we're depending on the original ewgi_context here!!!!
				case ewgi_api:request_method(Ctx) of
					'HEAD' ->
						ok = gen_tcp:close(Socket),
						GeneratorPid ! {discard, self()};
					_ ->
						wait_for_streamcontent_pid(Socket, GeneratorPid)
				end
		end
	after Timeout ->
		Req:respond({504, [], <<"Gateway Timeout">>})
	end.

%% Treat a stream with chunked transfer encoding
handle_stream(R, Generator) when is_function(Generator, 0) ->
    case (catch Generator()) of
	{H, T} when is_function(T, 0) ->
	    %% Prevent finishing the chunked response
	    case H of
		<<>> -> ok;
		[] -> ok;
		_ ->
		    R:write_chunk(H)
	    end,
	    handle_stream(R, T);
	{} ->
	    R:write_chunk([]);
	Error ->
	    error_logger:error_report(io_lib:format("Unexpected stream ouput (~p): ~p~n", [Generator, Error])),
	    R:write_chunk([])
    end;
handle_stream(R, Generator) ->
    error_logger:error_report(io_lib:format("Invalid stream generator: ~p~n", [Generator])),
    R:write_chunk([]).

%% Copied/adapted from yaws_server
wait_for_streamcontent_pid(CliSock, ContentPid) ->
    Ref = erlang:monitor(process, ContentPid),
    gen_tcp:controlling_process(CliSock, ContentPid),
    ContentPid ! {ok, self()},
    receive
        endofstreamcontent ->
	    ok = gen_tcp:close(CliSock),
            erlang:demonitor(Ref),
            %% should just use demonitor [flush] option instead?
            receive
                {'DOWN', Ref, _, _, _} ->
                    ok
            after 0 ->
                    ok
            end;
        {'DOWN', Ref, _, _, _} ->
            ok
    end,
    done.

%%--------------------------------------------------------------------
%% Push Streams API - copied from yaws_api
%% We could use mochiweb's write_chunk function but that
%% would require that we copy MochiResp around instead of
%% just copying the socket.
%%--------------------------------------------------------------------

%% This won't work for SSL for now
stream_process_deliver(Sock, IoList) ->
    gen_tcp:send(Sock, IoList).

%% This won't work for SSL for now either
stream_process_deliver_chunk(Sock, IoList) ->
    Chunk = case erlang:iolist_size(IoList) of
                0 ->
                    stream_process_deliver_final_chunk(Sock, IoList);
                S ->
                    [mochihex:to_hex(S), "\r\n", IoList, "\r\n"]
            end,
    gen_tcp:send(Sock, Chunk).
stream_process_deliver_final_chunk(Sock, IoList) ->
    Chunk = case erlang:iolist_size(IoList) of
                0 ->
                    <<"0\r\n\r\n">>;
                S ->
                    [mochihex:to_hex(S), "\r\n", IoList, "\r\n0\r\n\r\n"]
            end,
    gen_tcp:send(Sock, Chunk).

stream_process_end(Sock, ServerPid) ->
    gen_tcp:controlling_process(Sock, ServerPid),
    ServerPid ! endofstreamcontent.

%%--------------------------------------------------------------------

process_application(Appl, Ctx) when is_list(Appl) ->
    Path = ewgi_api:path_info(Ctx),
    process_mount_application(Ctx, Path, find_mount(Appl, Path));
process_application(Appl, Ctx) ->
    ewgi_application:run(Appl, Ctx).

process_mount_application(_, _, {not_found, _}) ->
    not_found;
process_mount_application(Ctx0, Path0, {MountPoint, Application}) ->
    Path = case Path0 of
               "*" -> "*";
               _ -> string:substr(Path0, length(MountPoint) + 1)
           end,
    Ctx = ewgi_api:path_info(Path, ewgi_api:script_name(MountPoint, Ctx0)),
    ewgi_application:run(Application, Ctx).

find_mount([], _) ->
    {not_found, fun (_, _) -> not_found end};
find_mount(Mounts, "*") ->
    lists:last(Mounts);
find_mount([{Path, _}=M|_], Path) ->
    M;
find_mount([{Point, _}=M|T], Path) ->
    case string:str(Path, Point ++ "/") of
        1 ->
            M;
        _ ->
            find_mount(T, Path)
    end.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

parse_arg(Req) ->
    ewgi_api:server_request_foldl(Req, fun parse_element/2, fun parse_ewgi_element/2, fun parse_http_header_element/2).

parse_element(auth_type, _Req) ->
    undefined;

parse_element(content_length, Req) ->
    case Req:get_header_value("content-length") of
        undefined -> undefined;
        Length when is_integer(Length) ->
            Length;
        Length when is_list(Length) ->
            list_to_integer(Length)
    end;

parse_element(content_type, Req) ->
    Req:get_header_value("content-type");

parse_element(gateway_interface, _Req) ->
    "EWGI/1.0";

parse_element(path_info, Req) ->
    RawPath = Req:get(raw_path),
    case RawPath of
        RawPath when RawPath =:= '*' ->
            "*";
        RawPath ->
            {_, _, Path, _, _} = mochiweb_util:urlsplit(RawPath),
            ewgi_api:unquote_path(Path)
    end;

%% Used to be:
%% filename:dirname(filename:dirname(code:which(Appl)))++Req:get(path); The
%% problem here is that the application only has a function which acts as the
%% entry point to the application.
parse_element(path_translated, _Req) ->
    undefined;

parse_element(query_string, Req) ->
    RawPath = Req:get(raw_path),
    case RawPath of
        RawPath when RawPath =:= '*' ->
            undefined;
        RawPath ->
            {_, _, _, QueryString, _} = mochiweb_util:urlsplit(RawPath),
            QueryString
    end;

parse_element(remote_addr, Req) ->
    Req:get(peer);

parse_element(remote_host, _Req) ->
    undefined;

parse_element(remote_ident, _Req) ->
    undefined;

parse_element(remote_user, _Req) ->
    undefined;

parse_element(request_method, Req) ->
    Req:get(method);

%% Default value is empty string. If mount points are used, SCRIPT_NAME
%% becomes the mount point.
parse_element(script_name, _Req) ->
    [];

parse_element(server_name, Req) ->
    HostPort = Req:get_header_value(host),
    case HostPort of
        HostPort when is_list(HostPort) ->
            hd(string:tokens(HostPort, ":"));
        HostPort -> HostPort
    end;

parse_element(server_port, Req) ->
    HostPort = Req:get_header_value(host),
    case HostPort of
        HostPort when is_list(HostPort) ->
            case length(HostPort) of
                2 -> lists:nth(2, HostPort);
                _ -> undefined
            end;
        _ ->
            undefined
    end;

parse_element(server_protocol, Req) ->
    {Maj, Min} = Req:get(version),
    lists:flatten(io_lib:format("HTTP/~b.~b", [Maj, Min]));

parse_element(server_software, _Req) ->
    "MochiWeb";

%% All other elements are undefined
parse_element(_, _) ->
    undefined.

parse_ewgi_element(read_input, Req) ->
    F = fun(Callback, Length) ->
                case Req:get_header_value("expect") of
                    "100-continue" ->
                        Req:start_raw_response({100, gb_trees:empty()});
                    _Else ->
                        ok
                end,
                read_input(Callback, Length, Req)
        end,
    F;

parse_ewgi_element(write_error, Req) ->
    F = fun(Msg) ->
                write_error(Msg, Req)
        end,
    F;

%% https?
parse_ewgi_element(url_scheme, _Req) ->
    "http";

parse_ewgi_element(version, _Req) ->
    {1, 0};

parse_ewgi_element(data, _Req) ->
    gb_trees:empty();

%% Ignore others
parse_ewgi_element(_, _) ->
    undefined.

parse_http_header_element(http_accept, Req) ->
    Req:get_header_value("accept");

parse_http_header_element(http_cookie, Req) ->
    Req:get_header_value("cookie");

parse_http_header_element(http_host, Req) ->
    Req:get_header_value("host");

parse_http_header_element(http_if_modified_since, Req) ->
    Req:get_header_value("if-modified-since");

parse_http_header_element(http_user_agent, Req) ->
    Req:get_header_value("user-agent");

parse_http_header_element(http_x_http_method_override, Req) ->
    Req:get_header_value("x-http-method-override");

parse_http_header_element(other, Req) ->
    lists:foldl(fun({K0, _}=Pair, Acc) ->
                        {K, V} = ewgi_api:normalize_header(Pair),
                        case K of
                            K when K =:= "content-length"
                            ; K =:= "content-type"
                            ; K =:= "accept"
                            ; K =:= "cookie"
                            ; K =:= "host"
                            ; K =:= "if-modified-since"
                            ; K =:= "user-agent"
                            ; K =:= "x-http-method-override" ->
                                Acc;
                            _ ->
                                Ex = case gb_trees:lookup(K, Acc) of
                                         {value, L} ->
                                             L;
                                         none ->
                                             []
                                     end,
                                gb_trees:insert(K, [{K0, V}|Ex], Acc)
                        end
                end, gb_trees:empty(), mochiweb_headers:to_list(Req:get(headers)));

parse_http_header_element(_, _) ->
    undefined.

%% No chunk size specified, so use default
read_input(Callback, Length, Req) when is_integer(Length) ->
    read_input(Callback, {Length, ?DEFAULT_CHUNKSIZE}, Req);

%% Final callback after entire input has been read
read_input(Callback, {Length, _ChunkSz}, _Req) when is_function(Callback), Length =< 0 ->
    Callback(eof);

%% Continue reading and calling back with each chunk of data
read_input(Callback, {Length, ChunkSz}, Req) when is_function(Callback) ->
    Bin = recv_input(Req, Length, ChunkSz),
    Rem = Length - size(Bin),
    NewCallback = Callback({data, Bin}),
    read_input(NewCallback, {Rem, ChunkSz}, Req).

%% Read either Length bytes or ChunkSz, whichever is smaller
recv_input(Req, Length, ChunkSz) when Length > 0, Length < ChunkSz ->
    Req:recv(Length);
recv_input(Req, _, ChunkSz) ->
    Req:recv(ChunkSz).

%% Write errors to error_logger
write_error(Msg, Req) ->
    error_logger:error_report([{message, Msg}, {request, Req}]).
