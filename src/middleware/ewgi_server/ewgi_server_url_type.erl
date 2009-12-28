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

%% special cases
discover_url_type(_SC, "" = _GetPath) ->
	#urltype{type=redir, scriptname = "/"};
discover_url_type(SC, "/" = GetPath) ->
	DocRoot = SC#sconf.docroot,
	case proplists:get_value("/", SC#sconf.appmods) of
		undefined ->
			FullPath =
				construct_fullpath(GetPath, DocRoot),
			maybe_return_dir(SC, GetPath, FullPath);
		{_,_,_} = MFA ->
			#urltype{type = appmod,
					 data = MFA,
					 scriptname = "/",
					 pathinfo = "",
					 fullpath = DocRoot}
	end;

discover_url_type(SC, GetPath) ->
	?Debug("GetPath = ~p~n", [GetPath]),
	DocRoot = SC#sconf.docroot,
	FullPath = construct_fullpath(GetPath, DocRoot),
	{Comps, RevFile} = comp_split(GetPath),
	?Debug("FullPath = ~p~n", [FullPath]),
	?Debug("Comps = ~p RevFile = ~p~n",[Comps, RevFile]),

	RequestSegs = string:tokens(GetPath,"/"),
	case active_appmod(SC#sconf.appmods, RequestSegs) of
		false ->
			case prim_file:read_file_info(FullPath) of
				{ok, FI} when FI#file_info.type == regular ->
					{Type, Mime} = suffix_type(SC, RevFile),
					#urltype{type=Type,
							 finfo=FI,
							 scriptname = GetPath,
							 pathinfo = "",
							 fullpath = FullPath,
							 mime=Mime};
				{ok, FI} when FI#file_info.type == directory ->
					case RevFile of
						[] ->
							maybe_return_dir(SC, GetPath, FullPath);
						_ ->
							%%Presence of RevFile indicates dir url
							%% had no trailing /
							#urltype{type=redir, scriptname = [GetPath, "/"]}
					end;
				_Err ->
					%% full path lookup failed but we might be dealing with a
					%% request = script_name + path_info so we have to check
					maybe_return_path_info(SC, Comps, RevFile)
			end;
			
		{ok, {Mount, {_,_,_} = MFA}} ->
			%%active_appmod found the most specific appmod for this
			%% request path
			%% - now we need to determine the path_info
			PathInfo = string:substr(GetPath, 1+length(Mount)),
			#urltype{
					 type = appmod,
					 data = MFA,
					 scriptname = Mount,
					 pathinfo = PathInfo
					}
	end.


maybe_return_dir(SC, GetPath, FullPath) ->
	IndexFiles = SC#sconf.index_files,
    case try_index_files(SC, GetPath, FullPath, IndexFiles) of
        noindex ->
            case file:list_dir(FullPath) of
                {ok, List} ->
                    #urltype{type = directory,
                             fullpath = FullPath,
							 scriptname = GetPath,
							 pathinfo = "",
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


%%active_appmod/2
%%find longest appmod match for request. (ie 'most specific' appmod)
%% - conceptually similar to the vdirpath scanning - but must also support
%% 'floating appmods' i.e an appmod specified as <path , appmodname> where
%% 'path' has no leading slash.
%%
%% a 'floating' appmod is not tied to a specific point in the URI structure
%% e.g for the configuration entry <myapp , myappAppmod>
%% the requests /docs/stuff/myapp/etc  & /otherpath/myapp   will both
%% trigger the myappAppmod module.
%% whereas for the configuration entry </docs/stuff/myapp , myappAppmod>
%% the request /otherpath/myapp will not trigger the appmod.
%%

active_appmod([], _RequestSegs) ->
    false;
active_appmod(AppMods, RequestSegs) ->

    %%!todo - review/test performance (e.g 'fun' calls are slower than a
    %% call to a local func - replace?)

    %%Accumulator is of form {RequestSegs, {AppmodMountPoint,Mod}}
    Matched =
        lists:foldl(
          fun(Pair,Acc) ->
                  {Mount, Mod, Excludes} = case Pair of
                                               {X, Y} -> {X, Y, []};
                                               {X,Y,Z} -> {X,Y,Z}
                                           end,
                  {ReqSegs, {LongestSoFar, _}} = Acc,

                  MountSegs = string:tokens(Mount,"/"),
                  case {is_excluded(ReqSegs, Excludes) ,
                        lists:prefix(MountSegs,ReqSegs)} of
                      {true, _} ->
                          Acc;
                      {false, true} ->
                          case LongestSoFar of
                              [$/|_] ->
                                  %%simple comparison of string length
                                  %% (as opposed to number of segments)
                                  %% should be ok here.
                                  if length(Mount) >
                                     length(LongestSoFar) ->
                                          {ReqSegs, {Mount, Mod}};
                                     true ->
                                          Acc
                                  end;
                              _ ->
                                  %%existing match is 'floating' -
                                  %% we trump it.

                                  {ReqSegs, {Mount, Mod}}
                          end;
                      {false, false} ->
                          case LongestSoFar of
                              [$/|_] ->
                                  %%There is already a match for an
                                  %% 'anchored' (ie absolute path)
                                  %% mount point.
                                  %% floating appmod can't override.
                                  Acc;
                              _ ->
                                  %%check for 'floating' match
                                  case lists:member(Mount, ReqSegs) of
                                      true ->
                                          %%!todo - review & document.
                                          %%latest 'floating' match wins
                                          %% if multiple match?
                                          %% (order in config vs position
                                          %% in request URI ?)

                                          {ReqSegs, {Mount, Mod}};
                                      false ->
                                          Acc
                                  end
                          end
                  end
          end, {RequestSegs, {"",""}}, AppMods),

    case Matched of
        {_RequestSegs, {"",""}} ->
            %%no appmod corresponding specifically to this http_request.path
            false;
        {_RequestSegs, {Mount, Mod}} ->
            {ok, {Mount, Mod}}
    end.

is_excluded(_, []) ->
    false;
is_excluded(RequestSegs, [ExcludeSegs|T]) ->
    case lists:prefix(ExcludeSegs, RequestSegs) of
        true ->
            true;
        false ->
            is_excluded(RequestSegs, T)
    end.


maybe_return_path_info(SC, Comps, RevFile) ->
	DR = SC#sconf.docroot,
    case path_info_split(Comps, DR) of
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
                     fullpath = FullPath,
                     scriptname = conc_path(HeadComps ++ [File]),
                     pathinfo = Trail,
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
path_info_split(Comps,DR) ->
    path_info_split(lists:reverse(Comps), DR, []).

path_info_split([H|T], DR, AccPathInfo) ->
    [$/|RevPath] = lists:reverse(H),
    case suffix_from_rev(RevPath) of
        [] ->   % shortcut clause, not necessary
            path_info_split(T, DR, [H|AccPathInfo]);
        Suff ->
            {Type, Mime} = ewgi_util_mime_types:t(Suff),
            case Type of
                regular ->
                    %%Don't hit the filesystem to test components that
                    %%'mime_types' indicates can't possibly be scripts
                    path_info_split(T, DR, [H|AccPathInfo]);
                X ->

                    %%We may still be in the 'PATH_INFO' section
                    %%Test to see if it really is a script

                    TestPath = lists:flatten(lists:reverse(T)),
                    FullPath = construct_fullpath(TestPath, DR) ++
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
                            path_info_split(T, DR, [H|AccPathInfo])
                    end
            end
    end;
path_info_split([], _DR_Vdir, _Acc) ->
    {not_a_script, error}.


%%%----------------------------------------------------------------------
%%%    Internal Functions
%%%----------------------------------------------------------------------
construct_fullpath(GetPath, DocRoot) ->
	DocRoot ++ GetPath.

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

