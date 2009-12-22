%%%----------------------------------------------------------------------
%%% File    : ewgi_server_url_type.erl (based on yaws_server.erl)
%%% Author  : Davide Marquês <nesrait@gmail.com>
%%% Purpose : Given a request pin down what to reply with.
%%% Created :  December 2009 by Davide Marquês <nesrait@gmail.com>
%%% Licensing information can be found in /licenses/yaws.LICENSE.
%%%----------------------------------------------------------------------

-module(ewgi_server_url_type).
-author('nesrait@gmail.com').

-include_lib("kernel/include/file.hrl").
-include("ewgi_server.hrl").

-export([discover_url_type/2]).

discover_url_type(SC, GetPath) ->
	DocRoot = SC#sconf.docroot,
	DocRootMountPoint = SC#sconf.docroot_mountpoint,
	FullPath = construct_fullpath(GetPath, DocRoot, DocRootMountPoint),
	?Debug("FullPath = ~p~n", [FullPath]),
	case GetPath of
		"/" -> %% special case
			case lists:keysearch("/", 1, SC#sconf.appmods) of
				{value, {_, Mod}} ->
					#urltype{type = appmod,
							 data = {Mod, []},
							 dir = "",
							 path = "",
							 fullpath = DocRoot};
				_ ->
					maybe_return_dir(SC, GetPath, FullPath)
			end;
		_ ->
			{Comps, RevFile} = comp_split(GetPath),
			?Debug("Comps = ~p RevFile = ~p~n",[Comps, RevFile]),

			_RequestSegs = string:tokens(GetPath,"/"),
			%% TODO:
			%%case active_appmod(SC#sconf.appmods, RequestSegs) of
			case false of
				false ->
					case prim_file:read_file_info(FullPath) of
						{ok, FI} when FI#file_info.type == regular ->
							{Type, Mime} = suffix_type(SC, RevFile),
							#urltype{type=Type,
									 finfo=FI,
									 dir = conc_path(Comps),
									 path = GetPath,
									 getpath = GetPath,
									 fullpath = FullPath,
									 mime=Mime};
						{ok, FI} when FI#file_info.type == directory ->
							case RevFile of
								[] ->
									maybe_return_dir(SC, GetPath, FullPath);
								_ ->
									%%Presence of RevFile indicates dir url
									%% had no trailing /
									#urltype{type=redir, path = [GetPath, "/"]}
							end;
						_Err ->
							%% full path lookup failed but we might be dealing with a
							%% request = script_name + path_info so we have to check
							maybe_return_path_info(SC, Comps, RevFile)
					end;
				{ok, {_Mount, _Mod}} ->
					%%active_appmod found the most specific appmod for this
					%% request path
					%% - now we need to determine the prepath & path_info
					%% TODO: 
					no_appmods_support_for_now
			end
	end.


maybe_return_dir(SC, GetPath, FullPath) ->
	IndexFiles = SC#sconf.index_files,
    case try_index_files(SC, GetPath, FullPath, IndexFiles) of
        noindex ->
            case file:list_dir(FullPath) of
                {ok, List} ->
                    #urltype{type = directory,
                             fullpath = FullPath,
                             dir = GetPath,
							 %% TODO: drop this?
                             data = List -- [".yaws_auth"]};
                _Err ->
                    #urltype{type=error}
            end;
        UT ->
            UT
    end.


try_index_files(_, _, _, []) ->
	noindex;
try_index_files(SC, GetPath, FullPath, [IndexFile|RemIndexFiles]) ->
    case prim_file:read_file_info([FullPath, IndexFile]) of
        {ok, FI} when FI#file_info.type == regular ->
            discover_url_type(SC, GetPath ++ IndexFile);
        _ ->
			try_index_files(SC, GetPath, FullPath, RemIndexFiles)
    end.


maybe_return_path_info(SC, Comps, RevFile) ->
	DR = SC#sconf.docroot,
	VirtualDir = SC#sconf.docroot_mountpoint,
    case path_info_split(Comps, {DR, VirtualDir}) of
        {not_a_script, error} ->
            %%can we use urltype.data to return more info?
            %% - logging?
            #urltype{type=error};
        {ok, FI, FullPath, HeadComps, File, TrailComps, Type, Mime} ->
            %%'File' is the only comp that has been returned without trailing "/"

            {Type2, Mime2} =
				case proplists:get_value(Type, SC#sconf.extension_mods) of
					undefined ->
                        {forbidden, "text/plain"};
					ExtModData ->
						{{extmod, ExtModData}, Mime}
                end,

            ?Debug("'script-selection' FullPath= ~p~n Mime=~p~n",
                   [FullPath, Mime2]),

            Trail = conc_path([ "/" ] ++ TrailComps ++
                              [ lists:reverse(RevFile) ]),


            #urltype{type = Type2,
                     finfo=FI,
                     dir =  conc_path(HeadComps),
                     path = conc_path(HeadComps ++ [File]),
                     fullpath = FullPath,
                     pathinfo = Trail,
                     getpath = case HeadComps of
                                   [] -> [$/|File];
                                   [_|_] ->
                                       conc_path(HeadComps ++ [File])
                               end,
                     mime = Mime2}
    end.
	

%%scan a list of 'comps' of form "pathsegment/"   (trailing slash always present)
%% - looking for the rightmost dotted component that corresponds to a script
%% file.

%% By the time path_info_split is called - the fullpath has already been tested
%%  and found not to be a file or directory
%%
%% Limitation: we don't support a script file without a dot.
%%  - otherwise we'd have to hit the filesystem for too many path components
%% to see if they exist & are an executable file.
%%
%% !!todo - review (potential security issue).
%% Right-to-left scanning should stop once we reach a 'document root mount
%% point', otherwise the Docroot that has been determined based on the full
%%  request path becomes invalid!
%%
path_info_split(Comps,DR_Vdir) ->
    path_info_split(lists:reverse(Comps), DR_Vdir, []).

path_info_split([H|T], {DR, VirtualDir}, AccPathInfo) ->
    [$/|RevPath] = lists:reverse(H),
    case suffix_from_rev(RevPath) of
        [] ->   % shortcut clause, not necessary
            path_info_split(T, {DR, VirtualDir}, [H|AccPathInfo]);
        Suff ->
            {Type, Mime} = ewgi_util_mime_types:t(Suff),
            case Type of
                regular ->
                    %%Don't hit the filesystem to test components that
                    %%'mime_types' indicates can't possibly be scripts
                    path_info_split(T, {DR, VirtualDir}, [H|AccPathInfo]);
                X ->

                    %%We may still be in the 'PATH_INFO' section
                    %%Test to see if it really is a script

                    TestPath = lists:flatten(lists:reverse(T)),
                    FullPath = construct_fullpath(TestPath, DR, VirtualDir) ++
                        string:strip(H,right,$/),

                    ?Debug("Testing for script at: ~p~n", [FullPath]),

                    case prim_file:read_file_info(FullPath) of
                        {ok, FI} when FI#file_info.type == regular ->
                            {ok, FI, FullPath, lists:reverse(T),
                             string:strip(H,right,$/), AccPathInfo, X, Mime};
                        {ok, FI} when FI#file_info.type == directory ->
                            %%just a case of a bad path starting at this point.
                            {not_a_script, error};
                        _Err ->
                            %%just looked like a script - keep going.
                            path_info_split(T, {DR, VirtualDir}, [H|AccPathInfo])
                    end
            end
    end;
path_info_split([], _DR_Vdir, _Acc) ->
    {not_a_script, error}.


%%%----------------------------------------------------------------------
%%%    Internal Functions
%%%----------------------------------------------------------------------
construct_fullpath(GetPath, DocRoot, DocRootMountPoint) ->
    case DocRootMountPoint of
        "/" ->
            DocRoot ++ GetPath;
        _ ->
            DocRoot ++ string:substr(GetPath, length(DocRootMountPoint) + 1)
    end.

suffix_type(SC, L) ->
    R=suffix_type(L),
	ExtensionMods = SC#sconf.extension_mods,
    case R of
        {regular, Ext, Mime} ->
            case proplists:get_value(Ext, ExtensionMods) of
				undefined ->
                    {regular, Mime};
				ExtModData ->
                    {{extmod, ExtModData}, Mime}
            end;
        {Script, _Ext, Mime} ->
            case proplists:get_value(Script, ExtensionMods) of
                undefined ->
					{forbidden, "text/plain"};
				ExtModData ->
                    {{extmod, ExtModData}, Mime}
            end
    end.

suffix_type(L) ->
    L2 = yaws:upto_char($., L),
    ewgi_util_mime_types:revt(L2).

%% comp_split/1 - split a path around "/" returning final segment as
%% reversed string.
%% return {Comps, RevPart} where Comps is a (possibly empty) list of path
%% components - always with trailing "/"
%% revPart is the final segment in reverse and has no "/".
%% e.g split( "/test/etc/index.html",[],[]) -> {["/test/", "etc/"], "lmth.xedni"}
%% revPart is useful in this form for looking up the file extension's mime-type.
%%
%% Terminology note to devs: reserve the word 'comp' to refer to a single
%% fragment of a path that we know has
%% a trailing slash. If you're dealing just with the part between slashes -
%% consider using the term 'segment' instead.
%% e.g  "x/" "/"   are all valid 'comps'
%% "/x" "/x/y/" "x" are not.
%%
comp_split(Path) ->
    do_comp_split(Path,[],[]).

%%when Part /= []
do_comp_split([$/|Tail], Comps, Part) ->
    NewComp = lists:reverse([$/|Part]),
    do_comp_split(Tail,  [NewComp | Comps], []);
do_comp_split([H|T], Comps, Part)  ->
    do_comp_split(T, Comps, [H|Part]);
do_comp_split([], Comps, Part) ->
    {lists:reverse(Comps), Part}.

%%conc_path
%% - single-level concatenatenation of a list of path components which
%% already contain slashes.
%% tests suggest it's significantly faster than lists:flatten or lists:concat
%% & marginally faster than lists:append (for paths of 3 or more segments anyway)
%% tested with various fairly short path lists - see src/benchmarks folder
%%

%%Original
conc_path([]) ->
    [];
conc_path([H|T]) ->
    H ++ conc_path(T).

suffix_from_rev(R) ->
    suffix_from_rev(R, []).

suffix_from_rev([$.|_], A) ->
    A;
suffix_from_rev([C|T], A) ->
    suffix_from_rev(T, [C|A]);
suffix_from_rev([], _A) ->
    [].
