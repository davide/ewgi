%%%----------------------------------------------------------------------
%%% File    : ewgi_server.erl
%%% Author  : Davide Marquês <nesrait@gmail.com>
%%% Purpose : 
%%% Created :  December 2009 by Davide Marquês <nesrait@gmail.com>
%%%----------------------------------------------------------------------

-module(ewgi_server).
-author('nesrait@gmail.com').

-include_lib("kernel/include/file.hrl").
-include("ewgi_server.hrl").

-export([run/2]).

run(Ctx0, [SC]) when is_record(SC, sconf) ->
	%% Remove DocRootMountPoint from GetPath
	DocRootMountPoint = SC#sconf.docroot_mountpoint,
	GetPath = ewgi_api:path_info(Ctx0),
	ServerGetPath = string:substr(GetPath, 1+length(DocRootMountPoint)),
	UT = ewgi_server_url_type:discover_url_type(SC, ServerGetPath),

	%% Add DocRootMountPoint and the discovered scriptname to the previous scriptname
	case ewgi_api:script_name(Ctx0) of
		undefined -> PrevScriptName = "";
		PrevScriptName -> ok
	end,
	ScriptName = PrevScriptName ++ DocRootMountPoint ++
											UT#urltype.scriptname,
	
	%% Set the new script_name and path_info
	Ctx = ewgi_api:script_name(ScriptName,
				ewgi_api:path_info(UT#urltype.pathinfo, Ctx0)),
	
	handle_url_type(SC, Ctx, UT);

run(Ctx, [DocRoot, DocRootMountPoint]) ->
	SC = #sconf{docroot=filename:absname(DocRoot),
						docroot_mountpoint=DocRootMountPoint},
	run(Ctx, [SC]).


handle_url_type(_, Ctx, #urltype{type=regular, fullpath=FullPath}) ->
	%% TODO: add a new ewgi return type to take advantage of the
	%% underlying web server's file serving/caching abilities.
	ewgi_stream_file:run(Ctx, [FullPath]);


handle_url_type(SC, Ctx, #urltype{type=directory} = UT) ->
	case SC#sconf.dir_content_listing of
		true ->
			GetPath = ewgi_api:script_name(Ctx),
			FullPath = UT#urltype.fullpath,
			Filenames = UT#urltype.data,
			ZipLinks = [zip],
			ewgi_dir_listing:list_dir(Ctx, GetPath, FullPath, Filenames, ZipLinks);
		_ ->
			send_403(Ctx, "text/html", "Directory listing denied")
	end;


handle_url_type(_, Ctx, #urltype{type=redir}) ->
	RedirUrl = ewgi_api:script_name(Ctx),
    ewgi_api:response_status({302, "Found"}, 
		ewgi_api:response_headers([{"Location", RedirUrl}], Ctx));


handle_url_type(_, Ctx, #urltype{type=forbidden, mime=Mime}) ->
	send_403(Ctx, Mime, "Execute access forbidden");


handle_url_type(SC, Ctx, #urltype{type={extmod,ExtModData}} = UT) ->
	{M, F, A} = ExtModData,
	NewUT = UT#urltype{type=undefined, data=A},
	apply(M, F, [SC, Ctx, NewUT]);


handle_url_type(_, Ctx, #urltype{type=appmod, data=MFA}) ->
	{M,F,A} = MFA,
	apply(M, F, [Ctx, A]);


handle_url_type(_, Ctx, UrlType) ->
	Msg = lists:flatten(io_lib:format("~p", [UrlType])),
	io:format("~p~n", [Msg]),
	send_404(Ctx, "text/html", Msg).


%%%----------------------------------------------------------------------
%%%    Response Functions
%%%----------------------------------------------------------------------
send_404(Ctx, Mime, Body) ->
	Status = {404, "Not Found"},
	send(Ctx, Status, Mime, Body).

send_403(Ctx, Mime, Body) ->
	Status = {403, "Forbidden"},
	send(Ctx, Status, Mime, Body).

send(Ctx, Status, Mime, Body) ->
	H = ewgi_api:response_headers(Ctx),
	CTHeader = {"Content-type", Mime},
	ewgi_api:response_headers([CTHeader|H],
		ewgi_api:response_status(Status,
			ewgi_api:response_message_body(Body, Ctx))).
