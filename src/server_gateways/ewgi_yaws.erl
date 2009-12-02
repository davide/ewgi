%%%-------------------------------------------------------------------
%%% File    : ewgi_yaws.erl
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
%%% <p>Reference implementation of a Yaws EWGI server gateway.</p>
%%%
%%% @end
%%%
%%% Created : 12 Oct 2007 by Filippo Pacini <filippo.pacini@gmail.com>
%%%-------------------------------------------------------------------
-module(ewgi_yaws).

-export([run/2]).
-export([
	 stream_process_deliver/2,
	 stream_process_deliver_chunk/2,
	 stream_process_deliver_final_chunk/2,
	 stream_process_end/2
	]).

-include_lib("yaws_api.hrl").
-include_lib("ewgi.hrl").

-define(INTERNAL_ERROR, [{status, 500}, {content, "text/plain", <<"Internal Server Error">>}]).
-define(BAD_REQUEST, [{status, 400}, {content, "text/plain", <<"Bad Request">>}]).

%%====================================================================
%% ewgi_server callbacks
%%====================================================================
run(Appl, Arg) ->
    try parse_arg(Arg) of
        Req when ?IS_EWGI_REQUEST(Req) ->
            Ctx0 = ewgi_api:context(Req, ewgi_api:empty_response()),
            try Appl(Ctx0) of
                Ctx when ?IS_EWGI_CONTEXT(Ctx) ->
                    handle_result(?INSPECT_EWGI_RESPONSE(Ctx), Arg#arg.clisock)
            catch
                _:Reason ->
                    error_logger:error_report(io_lib:format("Responding with 500 INTERNAL SERVER ERROR.~nReason: ~p~nStack: ~p~n", [Reason, erlang:get_stacktrace()])),
                    ?INTERNAL_ERROR
            end
    catch
        _:Reason ->
            error_logger:error_report(io_lib:format("Responding with 400 BAD REQUEST.~nReason: ~p~nStack: ~p~n", [Reason, erlang:get_stacktrace()])),
            ?BAD_REQUEST
    end.

handle_result(Ctx, Socket) ->
    case ewgi_api:response_message_body(Ctx) of
	{push_stream, GeneratorPid, Timeout} when is_pid(GeneratorPid) ->
	    GeneratorPid ! {push_stream_init, ?MODULE, self(), Socket},
	    receive
		{push_stream_init, GeneratorPid, Code, Headers, _TransferEncoding} ->
		    ContentType = get_content_type(Headers),
		    Acc = get_yaws_headers(Headers),
		    [{status, Code},
		     {allheaders, Acc},
		     {streamcontent_from_pid, ContentType, GeneratorPid}]
	    after Timeout ->
		    [{status, 504}, {content, "text/plain", <<"Gateway Timeout">>}]
	    end;
	Body ->
	    {Code, _} = ewgi_api:response_status(Ctx),
	    H = ewgi_api:response_headers(Ctx),
	    ContentType = get_content_type(H),
	    Acc = get_yaws_headers(H),
	    case Body of
		Generator when is_function(Generator, 0) ->
		    YawsPid = self(),
		    spawn(fun() -> handle_stream(Generator, YawsPid) end),
		    [{status, Code},
		     {allheaders, Acc},
		     {streamcontent_with_timeout, ContentType, <<>>, infinity}];
		_ ->
		    [{status, Code},
		     {allheaders, Acc},
		     {content, ContentType, Body}]
	    end
    end.

get_yaws_headers(H) ->
    lists:foldl(fun({K0, V}, Acc) ->
                        case string:to_lower(K0) of
                            "content-type" ->
                                Acc;
                            K ->
                                [{header, [K ++ ": ", V]}|Acc]
                        end
                end, [], H).

get_content_type(H) ->
    lists:foldl(fun({K, V}, Def) ->
                        case string:to_lower(K) of
                            "content-type" ->
                                V;
                            _ ->
                                Def
                        end
                end, "text/plain", H).

handle_stream(Generator, YawsPid) when is_function(Generator, 0) ->
    case (catch Generator()) of
        {H, T} when is_function(T, 0) ->
	    case H of
		<<>> -> ok;
		[] -> ok;
		_ ->
		    yaws_api:stream_chunk_deliver(YawsPid, [H])
	    end,
	    handle_stream(T, YawsPid);
	{} ->
	    yaws_api:stream_chunk_end(YawsPid);
	Error ->
	    error_logger:error_report(io_lib:format("Unexpected stream ouput (~p): ~p~n", [Generator, Error])),
	    yaws_api:stream_chunk_end(YawsPid)
    end;
handle_stream(Generator, YawsPid) ->
    error_logger:error_report(io_lib:format("Invalid stream generator: ~p~n", [Generator])),
    yaws_api:stream_chunk_end(YawsPid).

%%--------------------------------------------------------------------
%% Push Streams API
%%--------------------------------------------------------------------
stream_process_deliver(Socket, IoList) ->
	yaws_api:stream_process_deliver(Socket, IoList).

stream_process_deliver_chunk(Socket, IoList) ->
	yaws_api:stream_process_deliver_chunk(Socket, IoList).

stream_process_deliver_final_chunk(Socket, IoList) ->
	yaws_api:stream_process_deliver_final_chunk(Socket, IoList).

stream_process_end(Socket, ServerPid) ->
    yaws_api:stream_process_end(Socket, ServerPid).

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
parse_arg(Req) ->
    ewgi_api:server_request_foldl(Req, fun parse_element/2, fun parse_ewgi_element/2, fun parse_http_header_element/2).

parse_element(auth_type, #arg{headers=#headers{authorization=V}}) ->
    V;

parse_element(content_length, #arg{headers=#headers{content_length=V}}) ->
	case V of
		undefined -> 0;
		_ -> list_to_integer(V)
	end;

parse_element(content_type, #arg{headers=#headers{content_type=V}}) ->
    V;

parse_element(gateway_interface, _) ->
    "EWGI/1.0";

parse_element(path_info, #arg{pathinfo=undefined}) ->
    "/";
parse_element(path_info, #arg{pathinfo=V}) ->
    V;

parse_element(path_translated, #arg{fullpath=V}) ->
    V;

parse_element(query_string, #arg{querydata=V}) ->
    V;

parse_element(remote_addr, #arg{client_ip_port=Addr}) ->
    {{A,B,C,D}, _Port} = Addr,
    integer_to_list(A) ++ "."
        ++ integer_to_list(B) ++ "."
        ++ integer_to_list(C) ++ "."
        ++ integer_to_list(D);

parse_element(remote_host, _Req) ->
    undefined;

parse_element(remote_ident, _Req) ->
    undefined;

parse_element(remote_user, _Req) ->
    undefined;

parse_element(request_method, #arg{req=#http_request{method=V}}) ->
    V;

parse_element(script_name, #arg{prepath=V}) ->
    V;

parse_element(server_name, #arg{headers=#headers{host=HostPort}}) ->
    hd(string:tokens(HostPort, ":"));

parse_element(server_port, #arg{headers=#headers{host=HostPort0}}) ->
    HostPort = string:tokens(HostPort0, ":"),
    case length(HostPort) of
        2 -> [_H, P] = HostPort, P;
        _ -> undefined
    end;

parse_element(server_protocol, #arg{req=#http_request{version={Maj, Min}}}) ->
    "HTTP/" ++ integer_to_list(Maj) ++ "." ++ integer_to_list(Min);

parse_element(server_software, _) ->
    "Yaws";

parse_element(_, _) ->
    undefined.

%% TODO: Handle Expect: 100-Continue
parse_ewgi_element(read_input, #arg{clidata=Buf}) ->
    F = fun(Callback, Length) when is_integer(Length) -> % No chunk size specified, so use default
                read_input(Callback, {Length, ?DEFAULT_CHUNKSIZE}, Buf);
           (Callback, {Length, ChunkSz}) ->
                read_input(Callback, {Length, ChunkSz}, Buf)
        end,
    F;

parse_ewgi_element(write_error, Req) ->
    F = fun(Msg) ->
                error_logger:error_report([{message, Msg}, {request, Req}])
        end,
    F;

parse_ewgi_element(url_scheme, Req) ->
    case Req#arg.clisock of
	{sslsocket,_,_} -> "https";
	_ -> "http"
    end;

parse_ewgi_element(version, _) ->
    {1, 0};

parse_ewgi_element(data, _) ->
    gb_trees:empty();

parse_ewgi_element(_, _) ->
    undefined.

parse_http_header_element(http_accept, #arg{headers=#headers{accept=V}}) ->
    V;

parse_http_header_element(http_cookie, #arg{headers=#headers{cookie=[V]}}) ->
    V;
parse_http_header_element(http_cookie, #arg{headers=#headers{cookie=_}}) ->
    undefined;

parse_http_header_element(http_host, #arg{headers=#headers{host=V}}) ->
    V;

parse_http_header_element(http_if_modified_since, #arg{headers=#headers{if_modified_since=V}}) ->
    V;

parse_http_header_element(http_user_agent, #arg{headers=#headers{user_agent=V}}) ->
    V;

parse_http_header_element(http_x_http_method_override, #arg{headers=#headers{other=L}}) ->
    lists:foldl(fun({http_header,_,K0,_,V}, undefined) ->
                        K = case is_atom(K0) of
                                true ->
                                    atom_to_list(K0);
                                false ->
                                    K0
                            end,
                        case string:to_lower(K) of
                            "x-http-method-override" ->
                                V;
                            _ ->
                                undefined
                        end;
                   (_, Acc) ->
                        Acc
                end, undefined, L);

parse_http_header_element(other, #arg{headers=#headers{other=HOther}=H}) ->
    Dict0 = lists:foldl(fun(El, DAcc) ->
                                K0 = element(3, El),
                                {K, V} = ewgi_api:normalize_header({K0, element(5, El)}),
                                Ex = case gb_trees:lookup(K, DAcc) of
                                         {value, L} ->
                                             L;
                                         none ->
                                             []
                                     end,
                                gb_trees:insert(K, lists:reverse([{K0, V}|lists:reverse(Ex)]), DAcc)
                        end, gb_trees:empty(), HOther),
    FixedAuth =
	case H#headers.authorization of
	    undefined ->
		undefined;
	    {_Username, _Password, Auth} ->
		Auth
	end,
    Headers = [{"Connection", H#headers.connection},
	       {"If-Match", H#headers.if_match},
	       {"If-None-match", H#headers.if_none_match},
	       {"If-Range", H#headers.if_range},
	       {"If-Unmodified-Since", H#headers.if_unmodified_since},
	       {"Range", H#headers.range},
	       {"Referer", H#headers.referer},
	       {"Accept-Ranges", H#headers.accept_ranges},
	       {"Keep-Alive", H#headers.keep_alive},
	       {"Location", H#headers.location},
	       {"Content-Length", H#headers.content_length},
	       {"Content-Type", H#headers.content_type},
	       {"Content-Encoding", H#headers.content_encoding},
	       {"Authorization", FixedAuth},
	       {"Transfer-Encoding", H#headers.transfer_encoding}],
    DefinedHeaders = [{K,V} || {K,V} <- Headers, V /= undefined],
    lists:foldl(fun({K0, V0}, DAcc) ->
			{K, V} = ewgi_api:normalize_header({K0, V0}),
                        gb_trees:insert(K, [{K0, V}], DAcc)
                end, Dict0, DefinedHeaders).

%% Final callback after entire input has been read
read_input(Callback, {Length, _ChunkSz}, _Left) when is_function(Callback), Length =< 0 ->
    Callback(eof);

%% Continue reading and calling back with each chunk of data
read_input(Callback, {Length, ChunkSz}, Left) when is_function(Callback) ->
    L = recv_size(Length, ChunkSz),
    <<Bin:L/bytes,Rest/bits>> = Left,
    Rem = Length - size(Bin),
    NewCallback = Callback({data, Bin}),
    read_input(NewCallback, {Rem, ChunkSz}, Rest).

%% Read either Length bytes or ChunkSz, whichever is smaller
recv_size(Length, ChunkSz) when Length > 0, Length < ChunkSz ->
    Length;
recv_size(_, ChunkSz) ->
    ChunkSz.

