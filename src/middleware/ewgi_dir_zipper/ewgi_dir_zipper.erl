%%%----------------------------------------------------------------------
%%% File    : ewgi_dir_zipper.erl (based on yaws' yaws_ls.erl)
%%% Author  : Claes Wikstrom <klacke@hyber.org>
%%% Purpose : 
%%% Created :  5 Feb 2002 by Claes Wikstrom <klacke@hyber.org>
%%% Modified: 13 Jan 2004 by Martin Bjorklund <mbj@bluetail.com>
%%% Modified:    Jan 2006 by S꣡stien Bigot <sebastien.bigot@tremplin-utc.net>
%%% Refactored to ewgi_dir_zipper:
%%%                  December 2009 by Davide Marquês <nesrait@gmail.com>
%%% Licensing information can be found in /licenses/yaws.LICENSE.
%%%----------------------------------------------------------------------

-module(ewgi_dir_zipper).
-author('klacke@hyber.org').

-export([run/2]).

run(Ctx, Args) ->
	ZipperPid = spawn(fun() -> do_zip(Ctx, Args) end),
	ewgi_push_stream:run(Ctx, [ZipperPid]).

do_zip(Ctx, [Dir, ZipType]) ->
	do_zip(Ctx, [Dir, ZipType, "all"]);

do_zip(Ctx0, [Dir, ZipType, SuggestedFilename]) ->
	Filename = SuggestedFilename ++ "." ++ ZipType,
	case ZipType of
		"tgz" ->
			DoZip = fun tgz/2,
			CT = "application/gzip";
		"tbz2" ->
			DoZip = fun tbz2/2,
			CT = "application/gzip";
		_ -> %% "zip" ->
			DoZip = fun zip/2,
			CT = "application/zip"
	end,
	Status = {200, "OK"},
	H = ewgi_api:response_headers(Ctx0),
	AttachmentHeader = {"Content-Disposition",
									"attachment; filename=\"" ++ Filename ++ "\""},
	CTHeader = {"Content-type", CT},
	Ctx = ewgi_api:response_headers([AttachmentHeader, CTHeader|H],
				ewgi_api:response_status(Status, Ctx0)),
	case ewgi_api:stream_process_init(Ctx, chunked) of
		{ok, Connection} ->
			DoZip(Connection, Dir),
			ewgi_api:stream_process_end(Connection);
		_ -> ok
	end.

zip(Connection, Dir) ->
    process_flag(trap_exit, true),    
    P = open_port({spawn, "zip -q -1 -r - ."},
                  [{cd, Dir},use_stdio, binary, exit_status]),
    stream_loop(Connection, P).

tgz(Connection, Dir) ->
    process_flag(trap_exit, true),    
    P = open_port({spawn, "tar cz ."},
                  [{cd, Dir},use_stdio, binary, exit_status]),
    stream_loop(Connection, P).

tbz2(Connection, Dir) ->
    process_flag(trap_exit, true),    
    P = open_port({spawn, "tar cj ."},
                  [{cd, Dir},use_stdio, binary, exit_status]),
    stream_loop(Connection, P).

stream_loop(Connection, P) ->
    receive
        {P, {data, Data}} ->
            ewgi_api:stream_process_deliver_chunk(Connection, Data),
            stream_loop(Connection, P);
        {P, {exit_status, _}} ->
			ewgi_api:stream_process_deliver_final_chunk(Connection, "");
        {'EXIT', _UnusedServerPid, Status} ->
            exit(Status);
        Else ->
            error_logger:error_msg("Could not deliver zip file: ~p\n", [Else])
    end.

