%%%-------------------------------------------------------------------
%%% File    : ewgi_inets.erl
%%% Author  : Hunter Morris <huntermorris@gmail.com>
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
%%% <p>Reference implementation of a inets EWGI server gateway.</p>
%%% <p>Requires that environment variables 'app_module' and
%%% 'app_function' are set for application 'ewgi'</p>
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(ewgi_inets).

-export([do/1]).
-export([
		stream_process_deliver/2,
		stream_process_deliver_chunk/2,
		stream_process_deliver_final_chunk/2,
		stream_process_end/2
	]).

-include_lib("inets/src/httpd.hrl").
-include_lib("ewgi.hrl").

do(A) ->
    %% Taking advantage of the fact that all options are dumped to the config_db
    Appl = httpd_util:lookup(A#mod.config_db, ewgi_entry_app),
    try parse_arg(A) of
        Req when ?IS_EWGI_REQUEST(Req) ->
            try Appl(ewgi_api:context(Req, ewgi_api:empty_response())) of
                not_found ->
                    {proceed, [{response, {404, []}}]};
                Ctx when ?IS_EWGI_CONTEXT(Ctx) ->
                    handle_result(A, ?INSPECT_EWGI_RESPONSE(Ctx))
            catch
                _:Reason ->
                    error_logger:error_report(io_lib:format("Responding with 500 INTERNAL SERVER ERROR.~nReason: ~p~nStack: ~p~n", [Reason, erlang:get_stacktrace()])),
                    {break, [{response, {500, []}}]}
            end
    catch
        _:Reason ->
            error_logger:error_report(io_lib:format("Responding with 400 BAD REQUEST.~nReason: ~p~nStack: ~p~n", [Reason, erlang:get_stacktrace()])),
            {break, [{response, {400, []}}]}
    end.

parse_arg(A) when is_record(A, mod) ->
    ewgi_api:server_request_foldl(A, fun parse_element/2, fun parse_ewgi_element/2, fun parse_http_header_element/2).

parse_element(auth_type, _Req) ->
    undefined;

parse_element(content_length, #mod{parsed_header=H}) ->
    case proplists:get_value("content-length", H) of
        undefined -> undefined;
        Length when is_integer(Length) ->
            Length;
        Length when is_list(Length) ->
            list_to_integer(Length)
    end;

parse_element(content_type, #mod{parsed_header=H}) ->
    proplists:get_value("content-type", H);

parse_element(gateway_interface, _Req) ->
    "EWGI/1.0";

parse_element(path_info, #mod{request_uri=U}) ->
    {_, _, Path, _, _} = ewgi_api:urlsplit(U),
    ewgi_api:unquote_path(Path);

parse_element(path_translated, _Req) ->
    undefined;

parse_element(query_string, #mod{request_uri=U}) ->
    {_, _, _, QueryString, _} = ewgi_api:urlsplit(U),
    QueryString;

parse_element(remote_addr, #mod{socket_type={ssl, _}, socket=S}) ->
    get_ip(ssl:peername(S));
parse_element(remote_addr, #mod{socket=S}) ->
    get_ip(inet:peername(S));

parse_element(remote_host, _Req) ->
    undefined;

parse_element(remote_ident, _Req) ->
    undefined;

parse_element(remote_user, _Req) ->
    undefined;

parse_element(request_method, #mod{method=M}) ->
    get_method(M);

parse_element(script_name, _Req) ->
    [];

parse_element(server_name, #mod{parsed_header=H}) ->
    case proplists:get_value("host", H) of
        HostPort when is_list(HostPort) ->
            hd(string:tokens(HostPort, ":"));
        HostPort ->
            HostPort
    end;

parse_element(server_port, #mod{parsed_header=H}) ->
    case proplists:get_value("host", H) of
        Host when is_list(Host) ->
            case string:tokens(Host, ":") of
                [_, Port] ->
                    Port;
                _ ->
                    undefined
            end;
        _ ->
            undefined
    end;

parse_element(server_protocol, #mod{http_version=V}) ->
    V;

parse_element(server_software, _Req) ->
    "inets";

%% All other elements are undefined
parse_element(_, _) ->
    undefined.

parse_ewgi_element(read_input, #mod{entity_body=Buf}) ->
    F = fun(Callback, Length) when is_integer(Length) -> % No chunk size specified, so use default
                read_input(Callback, {Length, ?DEFAULT_CHUNKSIZE}, list_to_binary(Buf));
           (Callback, {Length, ChunkSz}) ->
                read_input(Callback, {Length, ChunkSz}, list_to_binary(Buf))
        end,
    F;

parse_ewgi_element(write_error, Req) ->
    F = fun(Msg) ->
                error_logger:error_report([{message, Msg}, {request, Req}])
        end,
    F;

parse_ewgi_element(url_scheme, #mod{socket_type={ssl, _}}) ->
    "https";
parse_ewgi_element(url_scheme, _) ->
    "http";

parse_ewgi_element(version, _) ->
    {1, 0};

parse_ewgi_element(data, A) ->
    gb_trees:from_orddict([{"inets.mod", A}]);

parse_ewgi_element(_, _) ->
    undefined.

get_header(K, #mod{parsed_header=H}) ->
    proplists:get_value(K, H).

parse_http_header_element(http_accept, A) ->
    get_header("accept", A);

parse_http_header_element(http_cookie, A) ->
    get_header("cookie", A);

parse_http_header_element(http_host, A) ->
    get_header("host", A);

parse_http_header_element(http_if_modified_since, A) ->
    get_header("if-modified-since", A);

parse_http_header_element(http_user_agent, A) ->
    get_header("user-agent", A);

parse_http_header_element(http_x_http_method_override, A) ->
    get_header("x-http-method-override", A);

parse_http_header_element(other, #mod{parsed_header=H}) ->
    lists:foldl(fun parse_other_header/2, gb_trees:empty(), H);

parse_http_header_element(_, _) ->
    undefined.

parse_other_header({K0, _}=Pair, Acc) ->
    parse_other_header1(K0, ewgi_api:normalize_header(Pair), Acc).

parse_other_header1(_, {"content-length", _}, Acc) ->
    Acc;
parse_other_header1(_, {"content-type", _}, Acc) ->
    Acc;
parse_other_header1(_, {"accept", _}, Acc) ->
    Acc;
parse_other_header1(_, {"cookie", _}, Acc) ->
    Acc;
parse_other_header1(_, {"host", _}, Acc) ->
    Acc;
parse_other_header1(_, {"if-modified-since", _}, Acc) ->
    Acc;
parse_other_header1(_, {"user-agent", _}, Acc) ->
    Acc;
parse_other_header1(_, {"x-http-method-override", _}, Acc) ->
    Acc;
parse_other_header1(K0, {K, V}, Acc) ->
    Ex = case gb_trees:lookup(K, Acc) of
             {value, L} -> L;
             none ->       []
         end,
    gb_trees:enter(K, [{K0, V}|Ex], Acc).

handle_result(#mod{config_db=Db}=A, Ctx) ->
	case ewgi_api:response_message_body(Ctx) of
		{push_stream, GeneratorPid, Timeout} when is_pid(GeneratorPid) ->
			ChunkedAllowed = not httpd_response:is_disable_chunked_send(Db),
			handle_push_stream(A, Ctx, ChunkedAllowed, GeneratorPid, Timeout);
		Body0 ->
			{Code, _} = ewgi_api:response_status(Ctx),
			Headers0 = [{string:to_lower(H), binary_to_list(iolist_to_binary(V))} || {H, V} <- ewgi_api:response_headers(Ctx)],
			Headers = lists:foldl(fun fold_header/2, [], Headers0),
			case Body0 of
				Body when is_function(Body, 0) ->
					ChunkedAllowed = not httpd_response:is_disable_chunked_send(Db),
					handle_result_wrap_stream(A, ChunkedAllowed, Code, Headers, Body);
				_ ->
					Body = [Body0],
				Length = {content_length, integer_to_list(erlang:iolist_size(Body))},
					{proceed, [{response, {response, [{code, Code}, Length] ++ Headers, Body}}]}
			end
	end.

handle_result_wrap_stream(#mod{http_version=Ver}, ChunkedAllowed, Code, Headers, Body0)
  when (Ver =/= "HTTP/1.1") or (not ChunkedAllowed) ->
    Body = stream_to_list(Body0),
    Length = {content_length, integer_to_list(erlang:iolist_size(Body))},
    {proceed, [{response, {response, [{code, Code}, Length] ++ Headers, Body}}]};
handle_result_wrap_stream(A, true, Code, Headers, Body) ->
    ExtraHeaders = httpd_response:cache_headers(A),
    httpd_response:send_header(A, Code, ExtraHeaders ++ [{transfer_encoding, "chunked"}|Headers]),
    handle_stream(A, Code, Body, 0).

handle_stream(A, Code, Generator, Size) when is_function(Generator, 0) ->
    case (catch Generator()) of
        {H, T} when is_function(T, 0) ->
	    case H of
		<<>> -> ok;
		[] -> ok;
		_ ->
		    httpd_response:send_chunk(A, [H], false)
	    end,
            handle_stream(A, Code, T, Size + erlang:iolist_size([H]));
        {} ->
            httpd_response:send_final_chunk(A, false),
	    {proceed, [{response, {already_sent, Code, Size}}]};
	Error ->
	    error_logger:error_report(io_lib:format("Unexpected stream ouput (~p): ~p~n", [Generator, Error])),
            httpd_response:send_final_chunk(A, false)
    end;
handle_stream(A, _Code, Generator, _Size) ->
    error_logger:error_report(io_lib:format("Invalid stream generator: ~p~n", [Generator])),
    httpd_response:send_final_chunk(A, false).

stream_to_list(S) when is_function(S, 0) ->
    case S() of
        {H, T} -> [H|stream_to_list(T)];
        {} ->     []
    end.

handle_push_stream(#mod{http_version=Ver}, _Ctx, ChunkedAllowed, GeneratorPid, _Timeout)
  when (Ver =/= "HTTP/1.1") or (not ChunkedAllowed) ->
	GeneratorPid ! {discard, self()},
    Body = "HTTP/1.1 required for streaming live data!",
    Length = {content_length, integer_to_list(erlang:iolist_size(Body))},
	NotSupported = 505,
    {proceed, [{response, {response, [{code, NotSupported}, Length], Body}}]};
handle_push_stream(A, Ctx, true, GeneratorPid, Timeout) ->
	Socket = A#mod.socket,
	GeneratorPid ! {push_stream_init, ?MODULE, self(), Socket},
	receive
	{push_stream_init, GeneratorPid, Code, Headers0, TransferEncoding} ->
		Headers1 = [{string:to_lower(H), binary_to_list(iolist_to_binary(V))} || {H, V} <- Headers0],
		Headers = lists:foldl(fun fold_header/2, [], Headers1),
		ExtraHeaders = httpd_response:cache_headers(A),
		httpd_response:send_header(A, Code, ExtraHeaders ++ Headers),
		Socket = A#mod.socket,
		case TransferEncoding of
			chunked ->
				wait_for_streamcontent_pid(Socket, GeneratorPid)
			;_ ->
				%% WARNING: we're depending on the original ewgi_context here!!!!
				case ewgi_api:request_method(Ctx) of
					'HEAD' ->
						case Socket of
							{sslsocket,_,_} ->
								ok = ssl:close(Socket);
							_ ->
								ok = gen_tcp:close(Socket)
						end,
						GeneratorPid ! {discard, self()};
					_ ->
						wait_for_streamcontent_pid(Socket, GeneratorPid)
				end	
		end
	after Timeout ->
		Body = "Gateway Timeout",
		Length = {content_length, integer_to_list(erlang:iolist_size(Body))},
		GatewayTimeout = 504,
		{proceed, [{response, {response, [{code, GatewayTimeout}, Length], Body}}]}
	end.

%% Copied/adapted from yaws_server
wait_for_streamcontent_pid(CliSock, ContentPid) ->
    Ref = erlang:monitor(process, ContentPid),
    case CliSock of
	{sslsocket,_,_} ->
	    ssl:controlling_process(CliSock, ContentPid);
	_ ->
	    gen_tcp:controlling_process(CliSock, ContentPid)
    end,
    ContentPid ! {ok, self()},
    receive
        endofstreamcontent ->
	    case CliSock of
		{sslsocket,_,_} ->
		    ok = ssl:close(CliSock);
		_ ->
		    ok = gen_tcp:close(CliSock)
	    end,
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
%%--------------------------------------------------------------------

stream_process_deliver(Sock={sslsocket,_,_}, IoList) ->
    ssl:send(Sock, IoList);
stream_process_deliver(Sock, IoList) ->
    gen_tcp:send(Sock, IoList).

stream_process_deliver_chunk(Sock, IoList) ->
    Chunk = case erlang:iolist_size(IoList) of
                0 ->
                    stream_process_deliver_final_chunk(Sock, IoList);
                S ->
                    [http_util:integer_to_hexlist(S), "\r\n", IoList, "\r\n"]
            end,
    stream_process_deliver(Sock, Chunk).
	
stream_process_deliver_final_chunk(Sock, IoList) ->
    Chunk = case erlang:iolist_size(IoList) of
                0 ->
                    <<"0\r\n\r\n">>;
                S ->
                    [http_util:integer_to_hexlist(S), "\r\n", IoList, "\r\n0\r\n\r\n"]
            end,
    stream_process_deliver(Sock, Chunk).

stream_process_end(Sock={sslsocket,_,_}, ServerPid) ->
    ssl:controlling_process(Sock, ServerPid),
    ServerPid ! endofstreamcontent;
stream_process_end(Sock, ServerPid) ->
    gen_tcp:controlling_process(Sock, ServerPid),
    ServerPid ! endofstreamcontent.

%%--------------------------------------------------------------------

%% What is the rationale behind this convertion?
%% If inets doesn't exclude unknown headers (see httpd_response:transform/1)
%% why should we do that here?
%% I suggest that we drop this header filtering altogether. (Davide)
fold_header({"accept-ranges", V}, Acc) ->
    [{accept_ranges, V}|Acc];
fold_header({"allow", V}, Acc) ->
    [{allow, V}|Acc];
fold_header({"cache-control", V}, Acc) ->
    [{cache_control, V}|Acc];
fold_header({"content-md5", V}, Acc) ->
    [{content_MD5, V}|Acc];
fold_header({"content-disposition", _}=H, Acc) ->
    [H|Acc];
fold_header({"content-encoding", V}, Acc) ->
    [{content_encoding, V}|Acc];
fold_header({"content-language", V}, Acc) ->
    [{content_language, V}|Acc];
fold_header({"content-length", V}, Acc) ->
    [{content_length, V}|Acc];
fold_header({"content-location", V}, Acc) ->
    [{content_location, V}|Acc];
fold_header({"content-range", V}, Acc) ->
    [{content_range, V}|Acc];
fold_header({"content-type", V}, Acc) ->
    [{content_type, V}|Acc];
fold_header({"date", V}, Acc) ->
    [{date, V}|Acc];
fold_header({"etag", V}, Acc) ->
    [{etag, V}|Acc];
fold_header({"expires", V}, Acc) ->
    [{expires, V}|Acc];
fold_header({"last-modified", V}, Acc) ->
    [{last_modified, V}|Acc];
fold_header({"location", V}, Acc) ->
    [{location, V}|Acc];
fold_header({"pragma", V}, Acc) ->
    [{pragma, V}|Acc];
fold_header({"retry-after", V}, Acc) ->
    [{retry_after, V}|Acc];
fold_header({"server", V}, Acc) ->
    [{server, V}|Acc];
fold_header({"set-cookie", _}=H, Acc) ->
    [H|Acc];
fold_header({"trailer", V}, Acc) ->
    [{trailer, V}|Acc];
fold_header({"transfer-encoding", V}, Acc) ->
    [{transfer_encoding, V}|Acc];
fold_header({"www-authenticate", _}=H, Acc) ->
    [H|Acc];
fold_header(_, Acc) ->
    %% Ignore unrecognised headers
    Acc.

get_ip({ok, {Ip, _Port}}) ->
    inet_parse:ntoa(Ip);
get_ip(_) ->
    undefined.

get_method("OPTIONS") ->
    'OPTIONS';
get_method("GET") ->
    'GET';
get_method("HEAD") ->
    'HEAD';
get_method("POST") ->
    'POST';
get_method("PUT") ->
    'PUT';
get_method("DELETE") ->
    'DELETE';
get_method("TRACE") ->
    'TRACE';
get_method("CONNECT") ->
    'CONNECT';
get_method(Other) ->
    Other.

%% Final callback after entire input has been read
read_input(Callback, {Length, _ChunkSz}, _Left) when is_function(Callback), Length =< 0 ->
    Callback(eof);

%% Continue reading and calling back with each chunk of data
read_input(Callback, {Length, ChunkSz}, Left) when is_function(Callback), is_binary(Left) ->
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

