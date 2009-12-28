%%%----------------------------------------------------------------------
%%% File    : ewgi_server_extension_mods.erl
%%% Author  : Davide Marquês <nesrait@gmail.com>
%%% Purpose : 
%%% Created :  December 2009 by Davide Marquês <nesrait@gmail.com>
%%%----------------------------------------------------------------------

-module(ewgi_server_extension_mods).
-author('nesrait@gmail.com').

-include("ewgi_server.hrl").

-export([
		cgi/3,
		interpreted_cgi/3,
		fastcgi/3,
		logged_download/3
	]).

cgi(SC, Ctx, #urltype{fullpath=Scriptfilename}) ->
	DocRoot = SC#sconf.docroot,
	DocRootMountPoint = SC#sconf.docroot_mountpoint,
	ewgi_cgi:call_cgi(Ctx, [DocRoot, DocRootMountPoint, Scriptfilename]).
													
													
interpreted_cgi(SC, Ctx, #urltype{fullpath=Scriptfilename,
												data=CGI_EXE}) ->
	DocRoot = SC#sconf.docroot,
	DocRootMountPoint = SC#sconf.docroot_mountpoint,
	ewgi_cgi:call_cgi(Ctx, [DocRoot, DocRootMountPoint,
													Scriptfilename, CGI_EXE]).


fastcgi(SC, Ctx, #urltype{fullpath=Scriptfilename,
									data=FCGI_Options}) ->
	DocRoot = SC#sconf.docroot,
	DocRootMountPoint = SC#sconf.docroot_mountpoint,
	ewgi_cgi:call_fcgi_responder(Ctx, [DocRoot, DocRootMountPoint,
														Scriptfilename, FCGI_Options]).


logged_download(_, Ctx, #urltype{fullpath=FullPath}) ->
	io:format("Sending: ~p!~n", [FullPath]),
	ewgi_stream_file:run(Ctx, [FullPath]).

