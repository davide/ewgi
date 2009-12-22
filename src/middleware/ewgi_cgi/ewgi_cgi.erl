%%%----------------------------------------------------------------------
%%% File    : ewgi_cgi.erl (based on yaws' yaws_cgi.erl)
%%% Original Authors:
%%%           carsten@codimi.de
%%%           brunorijsman@hotmail.com
%%% Purpose : Provide pluggable CGI/FastCGI support for ewgi.
%%% Converted to ewgi: December 2009 by Davide Marquês <nesrait@gmail.com>
%%% Licensing information can be found in /licenses/yaws.LICENSE.
%%%----------------------------------------------------------------------
-module(ewgi_cgi).
-author('carsten@codimi.de').
-author('brunorijsman@hotmail.com').         %% Added support for FastCGI
-author('nesrait@gmail.com').         %% Adapted the code for usage with EWGI

-include_lib("kernel/include/file.hrl").

-ifdef(debug).
-define(Debug(F, A),
	io:format("{debug, ~p, ~p, ~n   " ++ F ++ "}", [?FILE,?LINE| A])).
-else.
-define(Debug(_F, _A), ok).
-endif.

%%% Returns Out (i.e. same return values as out/1).
%%%
-export([call_cgi/2]).
-export([call_fcgi_responder/2]).

%%% Returns {allowed, Out} or {denied, Out}.
%%%
-export([call_fcgi_authorizer/2]).

%%% Returns [{VariableName, VariableValue}].
%%%
-export([fcgi_extract_variables/1]).

%%% TODO: Implement FastCGI filter role.

-export([cgi_worker/2, fcgi_worker/3]).

%%%==============================================================================
%%% Code shared between CGI and FastCGI
%%%==============================================================================

-define(ASCII_NEW_LINE, 10).
-define(ASCII_CARRIAGE_RETURN, 13).

isredirect(StatusCode) when is_integer(StatusCode),
StatusCode >301, StatusCode < 304 ->
    true;
isredirect(_) ->
    false.


checkdef(undefined) ->
    "";
checkdef(L) ->
    L.


to_list(L) when is_list(L) ->
    L;
to_list(A) when is_atom(A) ->
    atom_to_list(A).


get_socket_sockname(Socket={sslsocket,_,_}) ->
    {ok, {IP, _Port}}=ssl:sockname(Socket),
    inet_parse:ntoa(IP);
get_socket_sockname(Socket) ->
    {ok, {IP, _Port}}=inet:sockname(Socket),
    inet_parse:ntoa(IP).

build_env(Ctx, DocRoot, DocRootMountPoint,
	  Scriptname, Scriptfilename, ExtraEnv) ->
    Pathinfo = ewgi_api:path_info(Ctx),
    RequestURI = Scriptname ++ Pathinfo ++
	case ewgi_api:query_string(Ctx) of
	    undefined -> [];
	    [] -> [];
	    Q -> "?" ++ Q
	end,
    ServerSoftware = ewgi_api:server_software(Ctx),
    ServerProtocol = ewgi_api:server_protocol(Ctx),
    Hostname = ewgi_api:server_name(Ctx), %% H#headers.host
    Hostport = case ewgi_api:server_port(Ctx) of
		   undefined -> "80";
		   P -> P
	       end,

    PeerAddr = ewgi_api:remote_addr(Ctx),

    %% TODO: get access to the Clisock so that we can fecth this value
    %%LocalAddr = get_socket_sockname(Clisock),
    LocalAddr = "127.0.0.1",

    Scheme = ewgi_api:url_scheme(Ctx),
    %% Needed by trac, for redirs after POST
    HttpsEnv  = case Scheme of
                    "https" -> [{"HTTPS", "1"}];
                    _ ->[]
                end,

    %%determine what physical path the server would map Pathinfo
    %%to if it had received just Pathinfo in the request.
    PathTranslated = ewgi_api:path_translated(Ctx),

    Headers = ewgi_api:get_all_headers(Ctx),

    %% TODO: get back support for HTTP_AUTH
    AuthEnv = [],

    Extra_CGI_Vars0 = [], %% TODO: SC#sconf.extra_cgi_vars
    Extra_CGI_Vars = lists:flatmap(fun({Dir, Vars}) ->
					   case lists:prefix(Dir, Scriptfilename) of
					       true -> Vars;
					       false -> []
					   end
				   end,
				   Extra_CGI_Vars0),

    %%todo - review. should AuthEnv entries be overridable by ExtraEnv or not?
    %% we should define policy here rather than let through dupes.

    %% TODO: convert to list?
    RequestMethod = to_list(ewgi_api:request_method(Ctx)), %%yaws:to_list(R#http_request.method),
    QueryData = ewgi_api:query_string(Ctx), %% Arg#arg.querydata
    ContentType = ewgi_api:content_type(Ctx), %%H#headers.content_type
    ContentLength = ewgi_api:content_length(Ctx), %% H#headers.content_length

    %% TODO: extract+drop the headers mentioned down below?
    OtherHeaders = Headers,%% H#headers.other

    ExtraEnv ++
        HttpsEnv ++
        AuthEnv ++
        lists:filter(
          fun({K, L}) when is_list(L) ->
                  case lists:keysearch(K, 1, ExtraEnv) of
                      false ->
                          true;
                      _ ->
                          %% we have override in extraenv
                          false
                  end;
             (_) ->
                  false
          end,
          ([
            {"SERVER_SOFTWARE", ServerSoftware},
            {"SERVER_NAME", Hostname},
            {"HTTP_HOST", Hostname},
            {"GATEWAY_INTERFACE", "CGI/1.1"},
            {"SERVER_PROTOCOL", ServerProtocol},
            {"SERVER_PORT", Hostport},
            {"REQUEST_METHOD", RequestMethod},
            {"REQUEST_URI", RequestURI},
            {"DOCUMENT_ROOT", DocRoot},
            {"DOCUMENT_ROOT_MOUNT", DocRootMountPoint},
            {"SCRIPT_FILENAME", Scriptfilename},% For PHP 4.3.2 and higher
                                                % see http://bugs.php.net/bug.php?id=28227
                                                % (Sergei Golovan).
                                                % {"SCRIPT_TRANSLATED", Scriptfilename},   %IIS6+
            {"PATH_INFO",                Pathinfo},
            {"PATH_TRANSLATED",        PathTranslated},
            %% <JMN_2007-02>
            %%  CGI/1.1 spec says PATH_TRANSLATED should be NULL or unset
            %% if PATH_INFO is NULL
            %%  This is in contrast to IIS behaviour - and may break some apps.
            %%  broken apps that expect it to always correspond to path of
            %% script
            %%  should be modified to use SCRIPT_FILENAME instead - or wrapped.
            %% </JMN_2007-02>
            %% --------------------
            %%  <pre_2007-02_comments>
            %%  This seems not to
            %%  correspond to the
            %%  documentation I have
            %%  read, but it works
            %%  with PHP.
            %%
            %%  (Not with PHP 4.3.10-16) from
            %%  Debian sarge (Sergei Golovan).
            %%  </pre_2007-02_comments>
            %% ---------------------
            {"SCRIPT_NAME", Scriptname},
            {"REMOTE_ADDR", PeerAddr},
            {"REMOTE_HOST", PeerAddr},  %%  We SHOULD send this
            %%  Resolving DNS not practical for performance reasons
            %%  - at least on 1st contact from a particular host.
            %%  we could do background lookup so that it's available
            %% for subsequent invocations,
            %%  but it hardly seems worthwhile. We are permitted by the
            %% CGI/1.1 spec to substitute REMOTE_ADDR
            {"SERVER_ADDR", LocalAddr},   %% Apache compat
            {"LOCAL_ADDR", LocalAddr},    %% IIS compat
            {"QUERY_STRING", checkdef(QueryData)},
            {"CONTENT_TYPE", ContentType},
            {"CONTENT_LENGTH", ContentLength}
	    %%,
	    %%            {"HTTP_ACCEPT", H#headers.accept},
	    %%            {"HTTP_USER_AGENT", H#headers.user_agent},
	    %%            {"HTTP_REFERER", H#headers.referer},
	    %%            {"HTTP_IF_MODIFIED_SINCE", H#headers.if_modified_since},
	    %%            {"HTTP_IF_MATCH", H#headers.if_match},
	    %%            {"HTTP_IF_NONE_MATCH", H#headers.if_none_match},
	    %%            {"HTTP_IF_UNMODIFIED_SINCE", H#headers.if_unmodified_since},
	    %%            {"HTTP_COOKIE", flatten_val(make_cookie_val(H#headers.cookie))}
           ]++lists:map(fun({Var,Val})->{tohttp(Var),Val} end,
                        OtherHeaders)
          )) ++
	Extra_CGI_Vars.

tohttp(X) ->
    "HTTP_"++lists:map(fun tohttp_c/1, to_list(X)).


tohttp_c($-) ->
    $_;

tohttp_c(C) when C >= $a , C =< $z ->
    C - $a + $A;

tohttp_c(C) ->
    C.


make_cookie_val([]) ->
    undefined;
make_cookie_val([C]) ->
    C;
make_cookie_val([C|CS]) ->
    [make_cookie_val(CS), $; | C].


%%% Seems not to be necessary, but open_port documentation says that
%%% value has to be a string.

flatten_val(L) when is_list(L) ->
    lists:flatten(L);
flatten_val(X) ->
    X.


notslash($/) ->
    false;
notslash(_) ->
    true.


pathof(F) ->
    case lists:dropwhile(fun notslash/1, lists:reverse(F)) of
        "/" ->
            "/";
        [$/ | Tail] -> lists:reverse(Tail)
    end.


exeof(F) ->
    [$\., $/|lists:reverse(lists:takewhile(fun notslash/1, lists:reverse(F)))].


get_opt(Key, List, Default) ->
    case lists:keysearch(Key, 1, List) of
        {value, {_Key, Val}} -> Val;
        _ -> Default
    end.


%%%==============================================================================
%%% Code specific to CGI
%%%==============================================================================

%%%  TO DO:  Handle failure and timeouts.

%%%  call_cgi calls the script `Scriptfilename' (full path).
%%%  If `Exefilename' is given, it is the executable to handle this,
%%%  otherwise `Scriptfilame' is assumed to be executable itself.
%%%
%%%  Corresponding to a URI of
%%%     `http://somehost/some/dir/script.cgi/path/info',
%%%  `Pathinfo' should be set to `/path/info'.

%%%  These functions can be used from a `.yaws' file.
%%%  Note however, that they usually generate stream content.

call_cgi(Ctx, [DocRoot, DocRootMountPoint]) ->
    call_cgi(Ctx, [DocRoot, DocRootMountPoint, undefined, []]);

call_cgi(Ctx, [DocRoot, DocRootMountPoint, Exefilename]) ->
    call_cgi(Ctx, [DocRoot, DocRootMountPoint, Exefilename, []]);

call_cgi(Ctx, [_DocRoot, _DocRootMountPoint, _Exefilename, _ExtraEnv] = Args) ->
    WorkerPid = proc_lib:spawn(?MODULE, cgi_worker, [Ctx, Args]),
    ewgi_push_stream:run(Ctx, [WorkerPid]).

cgi_worker(Ctx, [DocRoot, DocRootMountPoint, Exefilename0, ExtraEnv]) ->
    Scriptname = ewgi_api:script_name(Ctx),
    RelPathToScript = string:substr(Scriptname, 1+length(DocRootMountPoint)),
    Scriptfilename = DocRoot ++ RelPathToScript,
    Exefilename = case Exefilename0 of
		      undefined -> exeof(Scriptfilename);
		      "" -> exeof(Scriptfilename);
		      FN -> FN
		  end,

    Env = build_env(Ctx, DocRoot, DocRootMountPoint,
		    Scriptname, Scriptfilename, ExtraEnv),
    ?Debug("~p~n", [Env]),
    CGIPort = open_port({spawn, Exefilename},
			[{env, Env},
			 {cd, pathof(Scriptfilename)},
			 exit_status,
			 binary]),
    cgi_pass_through_client_data(Ctx, CGIPort),
    cgi_do_work(CGIPort).

cgi_pass_through_client_data(Ctx, CGIPort) ->
    case ewgi_api:content_length(Ctx) of
	undefined -> L = 0;
	L -> ok
    end,
    R = ewgi_api:read_input(Ctx),
    R(cgi_pass_through_client_data(CGIPort), L).

cgi_pass_through_client_data(CGIPort) ->
    fun(eof) ->
            ?Debug("End of clidata~n", []);
       ({data, ClientData}) ->
	    ?Debug("Got clidata ~p~n", [binary_to_list(ClientData)]),
	    CGIPort ! {self(), {command, ClientData}},
            cgi_pass_through_client_data(CGIPort)
    end.


cgi_do_work(Port) ->
    cgi_header_loop({start, Port}, []).


cgi_header_loop(LineState, Headers) ->
    Line = cgi_get_line(LineState),
    ?Debug("Line = ~p~n", [Line]),
    case Line of
        {failure, Reason} ->
	    cgi_failure(Reason);
        {[], NewLineState} ->
	    cgi_headers_ready(NewLineState, Headers);
        {H, NewLineState} ->
            cgi_header_loop(NewLineState, [H|Headers])
    end.

cgi_headers_ready(LineState, RawHeaders) ->
    {Code, Headers} = 
	lists:foldl(fun(X, {Code, Headers}) ->
			    case do_header(X) of
				{status, NCode} -> {NCode, Headers};
				H -> {Code, [H|Headers]}
			    end
		    end,
		    {200, []},
		    RawHeaders),
    IsRedirect = isredirect(Code),
    case ewgi_api:stream_process_init(Code, Headers, chunked) of
	{ok, Connection} ->
	    case IsRedirect of
		true -> ok;
		false ->
		    case LineState of
			{middle, Data, Port} ->
			    ewgi_api:stream_process_deliver_chunk(Connection, Data),
			    cgi_data_loop(Connection, Port);
			{ending, Data, _} ->
			    ewgi_api:stream_process_deliver_final_chunk(Connection, Data)
		    end
	    end;
	discard ->
	    ok
    end.

do_header("HTTP/1."++[_,_,N1,N2,N3|_]) ->
    {status, list_to_integer([N1,N2,N3])};
do_header({"status", [N1,N2,N3|_], _}) ->
    {status, list_to_integer([N1,N2,N3])};
do_header(Header) when is_list(Header) ->
    [HdrName | HdrVal] = yaws:split_sep(Header, $:),
    HdrNmParts = [yaws:to_lower(H) || H <- yaws:split_sep(HdrName, $-)],
    HdrLower = yaws:join_sep(HdrNmParts, "-"),
    {HdrLower, HdrVal}.

cgi_data_loop(Connection, Port) ->
    receive
        {Port, {data,Data}} ->
            ?Debug("~p~n", [{data, binary_to_list(Data)}]),
            ewgi_api:stream_process_deliver_chunk(Connection, Data),
            cgi_data_loop(Connection, Port);
        {Port, {exit_status, _Status}} ->
            ?Debug("~p~n", [{exit_status, _Status}]),
	    ewgi_api:stream_process_deliver_final_chunk(Connection, ""),
            ewgi_api:stream_process_end(Connection);
        _Other ->
            ?Debug("~p~n", [_Other]),
            cgi_data_loop(Connection, Port)
    end.



cgi_get_line({start, Port}) ->
    receive
        {Port, {data,Data}} ->
            cgi_get_line([], {middle, Data, Port});
        {Port, {exit_status, 0}} ->
            ?Debug("~p~n", [{exit_status, 0}]),
            cgi_get_line([], {ending, <<>>, Port});
        {Port, {exit_status, Status}} when Status /=0 ->
            ?Debug("~p~n", [{exit_status, Status}]),
            {failure, {exit_status, Status}}
	    %%	This was removing the {push_stream_init,_,_,_} message from the queue
	    %%        ;_Other ->
	    %%            ?Debug("~p~n", [_Other]),
	    %%            cgi_get_line({start, Port})
    end;
cgi_get_line(State) ->
    cgi_get_line([], State).

cgi_get_line(Acc, {S, <<?ASCII_NEW_LINE, Tail/binary>>, Port}) ->
    {lists:reverse(Acc), {S, Tail, Port}};
cgi_get_line(Acc, {S, <<?ASCII_CARRIAGE_RETURN, ?ASCII_NEW_LINE, Tail/binary>>,
                   Port}) ->
    {lists:reverse(Acc), {S, Tail, Port}};
cgi_get_line(Acc, {middle, <<>>, Port}) ->
    cgi_get_line(Acc, cgi_add_resp(<<>>, Port));
cgi_get_line(Acc, {middle, <<?ASCII_CARRIAGE_RETURN>>, Port}) ->
    %% We SHOULD test for CRLF.
    %% Would be easier without.
    cgi_get_line(Acc, cgi_add_resp(<<?ASCII_CARRIAGE_RETURN>>, Port));
cgi_get_line(Acc, {ending, <<>>, Port}) ->
    {lists:reverse(Acc), {ending, <<>>, Port}};
cgi_get_line(Acc, {S, <<C, Tail/binary>>, Port}) ->
    cgi_get_line([C|Acc], {S, Tail, Port}).


cgi_add_resp(Bin, Port) ->
    receive
        {Port, {data,Data}} ->
            {middle, <<Bin/binary, Data/binary>>, Port};
        {Port, {exit_status, _Status}} ->
            ?Debug("~p~n", [{exit_status, _Status}]),
            {ending, Bin, Port};
        _Other ->
            ?Debug("~p~n", [_Other]),
            cgi_add_resp(Bin, Port)
    end.


%%%==============================================================================
%%% Code specific to FastCGI
%%%==============================================================================

-define(FCGI_VERSION_1, 1).

-define(FCGI_TYPE_BEGIN_REQUEST, 1).
-define(FCGI_TYPE_ABORT_REQUEST, 2).
-define(FCGI_TYPE_END_REQUEST, 3).
-define(FCGI_TYPE_PARAMS, 4).
-define(FCGI_TYPE_STDIN, 5).
-define(FCGI_TYPE_STDOUT, 6).
-define(FCGI_TYPE_STDERR, 7).
-define(FCGI_TYPE_DATA, 8).
-define(FCGI_TYPE_GET_VALUES, 9).
-define(FCGI_TYPE_GET_VALUES_RESULT, 10).
-define(FCGI_TYPE_UNKNOWN_TYPE, 11).

fcgi_type_name(?FCGI_TYPE_BEGIN_REQUEST) -> "begin-request";
fcgi_type_name(?FCGI_TYPE_ABORT_REQUEST) -> "abort-request";
fcgi_type_name(?FCGI_TYPE_END_REQUEST) -> "end-request";
fcgi_type_name(?FCGI_TYPE_PARAMS) -> "params";
fcgi_type_name(?FCGI_TYPE_STDIN) -> "stdin";
fcgi_type_name(?FCGI_TYPE_STDOUT) -> "stdout";
fcgi_type_name(?FCGI_TYPE_STDERR) -> "stderr";
fcgi_type_name(?FCGI_TYPE_DATA) -> "data";
fcgi_type_name(?FCGI_TYPE_GET_VALUES) -> "get_values";
fcgi_type_name(?FCGI_TYPE_GET_VALUES_RESULT) -> "get_values_result";
fcgi_type_name(?FCGI_TYPE_UNKNOWN_TYPE) -> "unknown-type";
fcgi_type_name(_) -> "?".

%%% The FCGI implementation does not support handling concurrent requests
%%% over a connection; it creates a separate connection for each
%%% request. Hence, all application records have the same request-id,
%%% namely 1.
%%%
-define(FCGI_REQUEST_ID_MANAGEMENT, 0).
-define(FCGI_REQUEST_ID_APPLICATION, 1).

-define(FCGI_DONT_KEEP_CONN, 0).
-define(FCGI_KEEP_CONN, 1).

-define(FCGI_ROLE_RESPONDER, 1).
-define(FCGI_ROLE_AUTHORIZER, 2).
-define(FCGI_ROLE_FILTER, 3).

-ifdef(debug).   % To avoid compile warning if debug is disabled.
fcgi_role_name(?FCGI_ROLE_RESPONDER) -> "responder";
fcgi_role_name(?FCGI_ROLE_AUTHORIZER) -> "authorizer";
fcgi_role_name(?FCGI_ROLE_FILTER) -> "filter";
fcgi_role_name(_) -> "?".
-endif.

-define(FCGI_STATUS_REQUEST_COMPLETE, 0).
-define(FCGI_STATUS_CANT_MPX_CONN, 1).
-define(FCGI_STATUS_OVERLOADED, 2).
-define(FCGI_STATUS_UNKNOWN_ROLE, 3).

fcgi_status_name(?FCGI_STATUS_REQUEST_COMPLETE) -> "request-complete";
fcgi_status_name(?FCGI_STATUS_CANT_MPX_CONN) -> "cannot-multiple-connection";
fcgi_status_name(?FCGI_STATUS_OVERLOADED) -> "overloaded";
fcgi_status_name(?FCGI_STATUS_UNKNOWN_ROLE) -> "unknown-role";
fcgi_status_name(_) -> "?".

%%% Amount of time (in milliseconds) allowed to connect to the application
%%% server.
%%%
-define(FCGI_CONNECT_TIMEOUT_MSECS, 10000).

%%% Amount of time (in milliseconds) allowed for data to arrive when
%%% reading the TCP connection to the application server.
%%%
-define(FCGI_READ_TIMEOUT_MSECS, 1000).

%%% TODO: Implement a configurable timeout which applies to the whole
%%% operation (as oposed to individual socket reads).

-record(fcgi_worker_state, {
	  app_server_host,            % The hostname or IP address of
						% the application server
	  app_server_port,            % The TCP port number of the
						% application server
	  env,                        % All environment variables to be passed
						% to the application (incl the extras)
	  keep_connection,            % Delegate close authority to the
						% application?
	  trace_protocol,             % If true, log info messages for sent
						% and received FastCGI messages
	  log_app_error,              % If true, log error messages for
						% application errors (stderr and
						% non-zero exit)
	  role,                       % The role of the worker
						% (responder, authorizer, filter)
	  app_server_socket,          % The TCP socket to the FastCGI
						% application server
	  stream_to_socket            % The TCP socket to the web browser
						% (stream chunked delivery to
						%  this socket)
	 }).


call_fcgi_responder(Ctx, [DocRoot, DocRootMountPoint]) ->
    call_fcgi_responder(Ctx, [DocRoot, DocRootMountPoint, []]);

call_fcgi_responder(Ctx, [_DocRoot, _DocRootMountPoint, _Options] = Args) ->
    call_fcgi(?FCGI_ROLE_RESPONDER, Ctx, Args).


call_fcgi_authorizer(Ctx, [DocRoot, DocRootMountPoint]) ->
    call_fcgi_authorizer(Ctx, [DocRoot, DocRootMountPoint, []]);
call_fcgi_authorizer(Ctx, [_DocRoot, _DocRootMountPoint, _Options] = Args) ->
    Out = call_fcgi(?FCGI_ROLE_AUTHORIZER, Ctx, Args),
    case fcgi_is_access_allowed(Out) of
        true ->
            StrippedOut = strip_content_from_out(Out),
            {allowed, StrippedOut};
        false ->
            {denied, Out}
    end.


call_fcgi(Role, Ctx, [_DocRoot, _DocRootMountPoint, _Options] = Args) ->
    WorkerPid = proc_lib:spawn(?MODULE, fcgi_worker, [Role, Ctx, Args]),
    ewgi_push_stream:run(Ctx, [WorkerPid]).


is_not_content({content, _MimeType, _Content}) -> false;
is_not_content({streamcontent, _MimeType, _Content}) -> false;
is_not_content(_) -> true.


strip_content_from_out(Out) ->
    lists:filter(fun is_not_content/1, Out).


fcgi_worker_fail(_WorkerState, Reason) ->
    cgi_failure(Reason),
    error_logger:error_msg("FastCGI failure: ~p~n", [Reason]),
    exit(Reason).

fcgi_worker_fail_if(true, WorkerState, Reason) ->
    fcgi_worker_fail(WorkerState, Reason);
fcgi_worker_fail_if(_Condition, _WorkerState, _Reason) ->
    ok.


fcgi_worker(Role, Ctx, [DocRoot, DocRootMountPoint, Options]) ->
    AppServerHost = proplists:get_value(app_server_host, Options),
    AppServerPort = proplists:get_value(app_server_port, Options),
    PreliminaryWorkerState = #fcgi_worker_state{},
    fcgi_worker_fail_if(AppServerHost == undefined, PreliminaryWorkerState,
                        app_server_host_must_be_configured),
    fcgi_worker_fail_if(AppServerPort == undefined, PreliminaryWorkerState,
                        app_server_port_must_be_configured),

    Scriptname = ewgi_api:script_name(Ctx),
    RelPathToScript = string:substr(Scriptname, 1+length(DocRootMountPoint)),
    Scriptfilename = DocRoot ++ RelPathToScript,
    ExtraEnv = get_opt(extra_env, Options, []),
    Env = build_env(Ctx, DocRoot, DocRootMountPoint,
		    Scriptname, Scriptfilename, ExtraEnv),
    TraceProtocol = proplists:get_value(trace_protocol, Options),
    LogAppError = proplists:get_value(log_app_error, Options),
    AppServerSocket =
	fcgi_connect_to_application_server(PreliminaryWorkerState,
					   AppServerHost, AppServerPort),
    ?Debug("Start FastCGI worker:~n"
	   "  Role = ~p (~s)~n"
	   "  AppServerHost = ~p~n"
	   "  AppServerPort = ~p~n"
	   "  ExtraEnv = ~p~n"
	   "  TraceProtocol = ~p~n"
	   "  LogAppStderr = ~p~n",
	   [Role, fcgi_role_name(Role),
	    AppServerHost,
	    AppServerPort,
	    ExtraEnv,
	    TraceProtocol,
	    LogAppError]),
    WorkerState = #fcgi_worker_state{
      app_server_host = AppServerHost,
      app_server_port = AppServerPort,
      env = Env,
      keep_connection = false,        % Currently hard-coded; make
						% configurable in the future?
      trace_protocol = TraceProtocol,
      log_app_error = LogAppError,
      role = Role,
      app_server_socket = AppServerSocket
     },
    fcgi_send_begin_request(WorkerState),
    fcgi_send_params(WorkerState, Env),
    fcgi_send_params(WorkerState, []),
    fcgi_pass_through_client_data(Ctx, WorkerState),
    fcgi_header_loop(WorkerState),
    gen_tcp:close(AppServerSocket),
    ok.

fcgi_pass_through_client_data(Ctx, WorkerState) ->
    case ewgi_api:content_length(Ctx) of
	undefined -> L = 0;
	L -> ok
    end,
    R = ewgi_api:read_input(Ctx),
    R(fcgi_pass_through_client_data(WorkerState), L).

fcgi_pass_through_client_data(WorkerState) ->
    fun(eof) ->
            ?Debug("End of clidata~n", []);
       ({data, ClientData}) ->
	    ?Debug("Got clidata ~p~n", [binary_to_list(ClientData)]),
	    fcgi_send_stdin(WorkerState, ClientData),
            fcgi_pass_through_client_data(WorkerState)
    end.


fcgi_connect_to_application_server(WorkerState, Host, Port) ->
    Options = [binary, {packet, 0}, {active, false}],
    case gen_tcp:connect(Host, Port, Options, ?FCGI_CONNECT_TIMEOUT_MSECS) of
        {error, Reason} ->
            fcgi_worker_fail(WorkerState, {connect_to_application_server_failed,
                                           Reason});
        {ok, Socket} ->
            Socket
    end.


fcgi_send_begin_request(WorkerState) ->
    KeepConnection = WorkerState#fcgi_worker_state.keep_connection,
    Flags = case KeepConnection of
		true -> ?FCGI_KEEP_CONN;
		false -> ?FCGI_DONT_KEEP_CONN
	    end,
    Role = WorkerState#fcgi_worker_state.role,
    fcgi_send_record(WorkerState, ?FCGI_TYPE_BEGIN_REQUEST,
                     ?FCGI_REQUEST_ID_APPLICATION, <<Role:16, Flags:8, 0:40>>).


fcgi_send_params(WorkerState, NameValueList) ->
    fcgi_send_record(WorkerState, ?FCGI_TYPE_PARAMS,
                     ?FCGI_REQUEST_ID_APPLICATION, NameValueList).


fcgi_send_stdin(WorkerState, Data) ->
    fcgi_send_record(WorkerState, ?FCGI_TYPE_STDIN,
                     ?FCGI_REQUEST_ID_APPLICATION, Data).


%%% Not needed yet
%%%
%%% fcgi_send_data(Socket, Data) ->
%%%     fcgi_send_record(Socket, ?FCGI_TYPE_DATA,
%%%                      ?FCGI_REQUEST_ID_APPLICATION, Data).


%%% Not needed yet
%%%
%%% fcgi_send_abort_request(Socket) ->
%%%     fcgi_send_record(Socket, ?FCGI_TYPE_ABORT_REQUEST,
%%%                      ?FCGI_REQUEST_ID_APPLICATION, <<>>).


fcgi_data_to_string(Data) ->
    fcgi_data_to_string("", 0, "", "", Data).

fcgi_data_to_string(LinesStr, Count, CharStr, HexStr, <<>>) ->
    if
        Count == 0 ->
            LinesStr;
        true ->
            Padding = lists:duplicate(16 - Count, $ ),
            LinesStr ++ "\n    " ++ CharStr ++ Padding ++ "  " ++ HexStr
    end;
fcgi_data_to_string(LinesStr, Count, CharStr, HexStr,
                    <<Byte:8, MoreData/binary>>) ->
    Char = if
	       (Byte >= $!) and (Byte =< $~) ->
		   Byte;
	       true ->
		   $.
end,
Hex = io_lib:format("~2.16.0b ", [Byte]),
if
    Count == 16 ->
	fcgi_data_to_string(LinesStr ++ "\n    " ++ CharStr ++ "  " ++
			    HexStr, 1, [Char], Hex, MoreData);
    true ->
	fcgi_data_to_string(LinesStr, Count + 1, CharStr ++ [Char],
			    HexStr ++ Hex, MoreData)
end.


fcgi_trace_protocol(WorkerState, Action, Version, Type, RequestId,
                    ContentLength, PaddingLength, Reserved, ContentData,
                    PaddingData) ->
    Trace = WorkerState#fcgi_worker_state.trace_protocol,
    if
        Trace ->
            error_logger:info_msg(
	      "~s FastCGI record:~n"
	      "  version = ~p~n"
	      "  type = ~p (~s)~n"
	      "  request-id = ~p~n"
	      "  content-length = ~p~n"
	      "  padding-length = ~p~n"
	      "  reserved = ~p~n"
	      "  content-data = ~s~n"
	      "  padding-data = ~s~n",
	      [Action,
	       Version,
	       Type, fcgi_type_name(Type),
	       RequestId,
	       ContentLength,
	       PaddingLength,
	       Reserved,
	       fcgi_data_to_string(ContentData),
	       fcgi_data_to_string(PaddingData)]);
        true ->
            ok
    end.


fcgi_send_record(WorkerState, Type, RequestId, NameValueList) ->
    EncodedRecord = fcgi_encode_record(WorkerState, Type, RequestId,
                                       NameValueList),
    AppServerSocket = WorkerState#fcgi_worker_state.app_server_socket,
    case gen_tcp:send(AppServerSocket, EncodedRecord) of
        {error, Reason} ->
            fcgi_worker_fail(WorkerState, {send_to_application_server_failed,
                                           Reason});
        ok ->
            ok
    end.


fcgi_encode_record(WorkerState, Type, RequestId, NameValueList)
  when is_list(NameValueList) ->
    fcgi_encode_record(WorkerState, Type, RequestId,
                       fcgi_encode_name_value_list(NameValueList));

fcgi_encode_record(WorkerState, Type, RequestId, ContentData)
  when is_binary(ContentData) ->
    Version = 1,
    ContentLength = size(ContentData),
    %% Add padding bytes (if needed) to content bytes to make
    %% content plus padding a multiple of 8 bytes.
    PaddingLength = if
                        ContentLength rem 8 == 0 ->
                            0;
                        true ->
                            8 - (ContentLength rem 8)
                    end,
    PaddingData = <<0:(PaddingLength * 8)>>,
    Reserved = 0,
    fcgi_trace_protocol(WorkerState, "Send", Version, Type, RequestId,
                        ContentLength, PaddingLength, Reserved,
                        ContentData, PaddingData),
    <<Version:8,
     Type:8,
     RequestId:16,
     ContentLength:16,
     PaddingLength:8,
     Reserved:8,
     ContentData/binary,
     PaddingData/binary>>.


fcgi_encode_name_value_list(_NameValueList = []) ->
    <<>>;
fcgi_encode_name_value_list(_NameValueList = [{Name, Value} | Tail]) ->
    <<(fcgi_encode_name_value(Name,Value))/binary,
     (fcgi_encode_name_value_list(Tail))/binary>>.


fcgi_encode_name_value(Name, _Value = undefined) ->
    fcgi_encode_name_value(Name, "");
fcgi_encode_name_value(Name, Value) when is_list(Name) and is_list(Value) ->
    NameSize = length(Name),
    %% If name size is < 128, encode it as one byte with the high bit clear.
    %% If the name size >= 128, encoded it as 4 bytes with the high bit set.
    NameSizeData = if
                       NameSize < 128 ->
                           <<NameSize:8>>;
                       true ->
                           <<(NameSize bor 16#80000000):32>>
                               end,
    %% Same encoding for the value size.
    ValueSize = length(Value),
    ValueSizeData = if
			ValueSize < 128 ->
			    <<ValueSize:8>>;
			true ->
			    <<(ValueSize bor 16#80000000):32>>
				end,
    <<NameSizeData/binary,
     ValueSizeData/binary,
     (list_to_binary(Name))/binary,
     (list_to_binary(Value))/binary>>.


fcgi_header_loop(WorkerState) ->
    fcgi_header_loop(WorkerState, start, []).

fcgi_header_loop(WorkerState, LineState, Headers) ->
    Line = fcgi_get_line(WorkerState, LineState),
    case Line of
        {failure, Reason} ->
            cgi_failure(Reason);
	{[], NewLineState} ->
	    fcgi_headers_ready(WorkerState, NewLineState, Headers);
        {H, NewLineState} ->
            fcgi_header_loop(WorkerState, NewLineState, [H|Headers])
    end.

fcgi_headers_ready(WorkerState, LineState, RawHeaders) ->
    {Code, Headers} = 
	lists:foldl(fun(X, {Code, Headers}) ->
			    case do_header(X) of
				{status, NCode} -> {NCode, Headers};
				H -> {Code, [H|Headers]}
			    end
		    end,
		    {200, []},
		    RawHeaders),
    IsRedirect = isredirect(Code),
    case ewgi_api:stream_process_init(Code, Headers, chunked) of
	{ok, Connection} ->
	    case IsRedirect of
		true -> ok;
		false ->
		    case LineState of
			{middle, Data} ->
			    ewgi_api:stream_process_deliver_chunk(Connection, Data),
			    fcgi_data_loop(Connection, WorkerState);
			{ending, Data} ->
			    ewgi_api:stream_process_deliver_final_chunk(Connection, Data)
		    end
	    end;
	discard ->
	    ok
    end.

fcgi_get_line(WorkerState, start) ->
    case fcgi_get_output(WorkerState) of
        {data, Data} ->
            fcgi_get_line(WorkerState, [], {middle, Data});
        {exit_status, 0} ->
            fcgi_get_line(WorkerState, [], {ending, <<>>});
        {exit_status, Status} when Status /=0 ->
            {failure, {exit_status, Status}}
    end;
fcgi_get_line(WorkerState, LineState) ->
    fcgi_get_line(WorkerState, [], LineState).

fcgi_get_line(_WorkerState, Acc, {State, <<?ASCII_NEW_LINE, Tail/binary>>}) ->
    {lists:reverse(Acc), {State, Tail}};
fcgi_get_line(_WorkerState, Acc, {State, <<?ASCII_CARRIAGE_RETURN,
					  ?ASCII_NEW_LINE, Tail/binary>>}) ->
    {lists:reverse(Acc), {State, Tail}};
fcgi_get_line(WorkerState, Acc, {middle, <<>>}) ->
    fcgi_get_line(WorkerState, Acc, fcgi_add_resp(WorkerState, <<>>));
fcgi_get_line(WorkerState, Acc, {middle, <<?ASCII_CARRIAGE_RETURN>>}) ->
    fcgi_get_line(WorkerState, Acc, fcgi_add_resp(WorkerState,
                                                  <<?ASCII_CARRIAGE_RETURN>>));
fcgi_get_line(_WorkerState, Acc, {ending, <<>>}) ->
    {lists:reverse(Acc), {ending, <<>>}};
fcgi_get_line(WorkerState, Acc, {State, <<Char, Tail/binary>>}) ->
    fcgi_get_line(WorkerState, [Char | Acc], {State, Tail}).


fcgi_add_resp(WorkerState, OldData) ->
    case fcgi_get_output(WorkerState) of
        {data, NewData} ->
            {middle, <<OldData/binary, NewData/binary>>};
        {exit_status, _Status} ->
            {ending, OldData}
    end.


fcgi_data_loop(Connection, WorkerState) ->
    case fcgi_get_output(WorkerState) of
        {data, Data} ->
            ewgi_api:stream_process_deliver_chunk(Connection, Data),
            fcgi_data_loop(Connection, WorkerState);
        {exit_status, _Status} ->
            ewgi_api:stream_process_end(Connection)
    end.

fcgi_get_output(WorkerState) ->
    {Type, ContentData} = fcgi_receive_record(WorkerState),
    case Type of
        ?FCGI_TYPE_END_REQUEST ->
            <<AppStatus:32/signed, ProtStatus:8, _Reserved:24>> = ContentData,
            fcgi_worker_fail_if(ProtStatus < ?FCGI_STATUS_REQUEST_COMPLETE,
                                WorkerState,
                                {received_unknown_protocol_status, ProtStatus}),
            fcgi_worker_fail_if(ProtStatus > ?FCGI_STATUS_UNKNOWN_ROLE,
                                WorkerState,
                                {received_unknown_protocol_status, ProtStatus}),
            if
                ProtStatus /= ?FCGI_STATUS_REQUEST_COMPLETE ->
                    error_logger:error_msg("FastCGI protocol error: ~p (~s)~n",
                                           ProtStatus,
                                           fcgi_status_name(ProtStatus));
                true ->
                    ok
            end,
            if
                (AppStatus /= 0),
                (WorkerState#fcgi_worker_state.log_app_error) ->
                    error_logger:error_msg(
                      "FastCGI application non-zero exit status: ~p~n",
                      [AppStatus]);
                true ->
                    ok
            end,
            {exit_status, AppStatus};
        ?FCGI_TYPE_STDOUT ->
            {data, ContentData};
        ?FCGI_TYPE_STDERR ->
            if
                (ContentData /= <<>>),
                (WorkerState#fcgi_worker_state.log_app_error) ->
                    error_logger:error_msg(
                      "FastCGI application stderr output:~s~n",
                      [fcgi_data_to_string(ContentData)]);
                true ->
                    ok
            end,
            fcgi_get_output(WorkerState);
        ?FCGI_TYPE_UNKNOWN_TYPE ->
            <<UnknownType:8, _Reserved:56>> = ContentData,
            fcgi_worker_fail(
              WorkerState,
              {application_did_not_understand_record_type_we_sent, UnknownType});
        OtherType ->
            fcgi_worker_fail(WorkerState,
                             {received_unknown_record_type, OtherType})
    end.


fcgi_receive_record(WorkerState) ->
    {ok, Header} = fcgi_receive_binary(WorkerState, 8, ?FCGI_READ_TIMEOUT_MSECS),
    <<Version:8, Type:8, RequestId:16, ContentLength:16,
     PaddingLength:8, Reserved:8>> = Header,
    fcgi_worker_fail_if(Version /= 1, WorkerState,
                        {received_unsupported_version, Version}),
    case Type of
        ?FCGI_TYPE_END_REQUEST ->
            fcgi_worker_fail_if(RequestId /= ?FCGI_REQUEST_ID_APPLICATION,
                                WorkerState,
                                {unexpected_request_id, RequestId}),
            fcgi_worker_fail_if(ContentLength /= 8, WorkerState,
                                {incorrect_content_length_for_end_request,
                                 ContentLength}),
            ok;
        ?FCGI_TYPE_STDOUT ->
            fcgi_worker_fail_if(RequestId /= ?FCGI_REQUEST_ID_APPLICATION,
                                WorkerState,
                                {unexpected_request_id, RequestId}),
            ok;
        ?FCGI_TYPE_STDERR ->
            fcgi_worker_fail_if(RequestId /= ?FCGI_REQUEST_ID_APPLICATION,
                                WorkerState,
                                {unexpected_request_id, RequestId}),
            ok;
        ?FCGI_TYPE_UNKNOWN_TYPE ->
            fcgi_worker_fail_if(RequestId /= ?FCGI_REQUEST_ID_MANAGEMENT,
                                WorkerState,
                                {unexpected_request_id, RequestId}),
            fcgi_worker_fail_if(ContentLength /= 8, WorkerState,
                                {incorrect_content_length_for_unknown_type,
                                 ContentLength}),
            ok;
        OtherType ->
            throw({received_unexpected_type, OtherType})
    end,
    case fcgi_receive_binary(WorkerState, ContentLength,
                             ?FCGI_READ_TIMEOUT_MSECS) of
        {error, Reason} ->
            fcgi_worker_fail(WorkerState, {unable_to_read_content_data, Reason});
        {ok, ContentData} ->
            case fcgi_receive_binary(WorkerState, PaddingLength,
                                     ?FCGI_READ_TIMEOUT_MSECS) of
                {error, Reason} ->
                    fcgi_worker_fail(
                      WorkerState,
                      {unable_to_read_record_padding_data, Reason});
                {ok, PaddingData} ->
                    fcgi_trace_protocol(WorkerState, "Receive",
                                        Version, Type, RequestId, ContentLength,
                                        PaddingLength, Reserved, ContentData,
                                        PaddingData),
                    {Type, ContentData}
            end
    end.


fcgi_receive_binary(_WorkerState, Length, _Timeout) when Length == 0 ->
    {ok, <<>>};
fcgi_receive_binary(WorkerState, Length, Timeout) ->
    AppServerSocket = WorkerState#fcgi_worker_state.app_server_socket,
    case gen_tcp:recv(AppServerSocket, Length, Timeout) of
        {error, Reason} ->
            fcgi_worker_fail(WorkerState, {send_to_application_server_failed, Reason});
        {ok, Data} ->
            {ok, Data}
    end.


%%% Access is allowed if, and only if, the resonse from the authorizer
%%% running on the application server contains a 200 OK status. Any other
%%% status or absence of a status means access is denied.
%%%
fcgi_is_access_allowed([Head | Tail]) ->
    fcgi_is_access_allowed(Head) orelse fcgi_is_access_allowed(Tail);
fcgi_is_access_allowed({status, 200}) ->
    true;
fcgi_is_access_allowed(_AnythingElse) ->
    false.


%%% Look for headers of the form "Variable-VAR_NAME: var value"
%%%
fcgi_extract_variables([Head | Tail]) ->
    fcgi_extract_variables(Head) ++ fcgi_extract_variables(Tail);
fcgi_extract_variables({header, "Variable-" ++ Rest}) ->
    [fcgi_split_header(Rest)];
fcgi_extract_variables(_AnythingElse) ->
    [].


fcgi_split_header(Header) ->
    fcgi_split_header(name, [], [], Header).

fcgi_split_header(_, NameAcc, ValueAcc, "") ->
    {string:strip(lists:reverse(NameAcc)),
     string:strip(lists:reverse(ValueAcc))};
fcgi_split_header(name, NameAcc, ValueAcc, [$: | MoreStr]) ->
    fcgi_split_header(value, NameAcc, ValueAcc, MoreStr);
fcgi_split_header(name, NameAcc, ValueAcc, [Char | MoreStr]) ->
    fcgi_split_header(name, [Char | NameAcc], ValueAcc, MoreStr);
fcgi_split_header(value, NameAcc, ValueAcc, [Char | MoreStr]) ->
    fcgi_split_header(value, NameAcc, [Char | ValueAcc], MoreStr).

cgi_failure(Reason) ->
    StatusCode = 500,
    Headers = [{"Content-type", "text/html"}],
    CGIFailure = io_lib:format("CGI failure: ~p", [Reason]),
    ContLen = iolist_size(CGIFailure),
    case ewgi_api:stream_process_init(StatusCode, Headers, ContLen) of
	{ok, Connection} ->
	    ewgi_api:stream_process_deliver_final_chunk(Connection, CGIFailure);
	discard ->
	    ok
    end.
