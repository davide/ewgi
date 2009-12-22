%%%----------------------------------------------------------------------
%%% File    : ewgi_dir_listing.erl (based on yaws' yaws_ls.erl)
%%% Author  : Claes Wikstrom <klacke@hyber.org>
%%% Purpose : 
%%% Created :  5 Feb 2002 by Claes Wikstrom <klacke@hyber.org>
%%% Modified: 13 Jan 2004 by Martin Bjorklund <mbj@bluetail.com>
%%% Modified:    Jan 2006 by Sébastien Bigot <sebastien.bigot@tremplin-utc.net>
%%% Refactored to ewgi_dir_listing:
%%%                  December 2009 by Davide MarquÃªs <nesrait@gmail.com>
%%% Licensing information can be found in /licenses/yaws.LICENSE.
%%%----------------------------------------------------------------------

-module(ewgi_dir_listing).
-author('klacke@hyber.org').
-author('nesrait@gmail.com').    %% Adapted the code for usage with EWGI

-include_lib("kernel/include/file.hrl").

-export([run/2]).
-export([list_dir/5]).

-define(F(Format,Args),
	io_lib:format(Format,Args)).
-define(FILE_LEN_SZ, 45).

run(Ctx, [GetPath, DocRoot, DocRootMountPoint]) ->
    ZipLinks = [zip], %%[zip, tgz, tbz2],
    run(Ctx, [GetPath, DocRoot, DocRootMountPoint, ZipLinks]);

run(Ctx, [GetPath, DocRoot, DocRootMountPoint, ZipLinks]) ->
	RelPath = string:substr(GetPath, 1+length(DocRootMountPoint)),
	FullPath = DocRoot ++ RelPath,
	{ok, Filenames} = file:list_dir(FullPath),
	list_dir(Ctx, GetPath, FullPath, Filenames, ZipLinks).

%% Expects GetPath to end with a $/
list_dir(Ctx, GetPath, FullPath, Filenames, ZipLinks) ->
	case ewgi_api:query_string(Ctx) of
	[$a,$l,$l,$.|ZipType] ->
		ewgi_dir_zipper:run(Ctx, [FullPath, ZipType]);
	[$i,$c,$o,$n,$s,$/|Icon] ->
		serve_icon(Ctx, Icon);
	_ ->
		actual_list_dir(Ctx, GetPath, FullPath, Filenames, ZipLinks)
	end.


actual_list_dir(Ctx, GetPath, FullPath, Filenames, ZipLinks) ->

    {Pos, Direction, Qry} = parse_query(Ctx),

    Descriptions = read_descriptions(FullPath),

    L0 = lists:zf(
           fun(F) ->
                   File = FullPath ++ [$/|F],
                   FI = file:read_file_info(File),
                   file_entry(FI, GetPath, F, Qry,Descriptions)
           end, Filenames),

    L1 = lists:keysort(Pos, L0),

    L2 = if Direction == normal -> L1;
            Direction == reverse -> lists:reverse(L1)
         end,

    L3 = [Html || {_, _, _, _, Html} <- L2],

    Body = [ doc_head(GetPath),                          
             dir_header(FullPath, GetPath),
             table_head(GetPath, Direction),             
             parent_dir(),
             do_zip_links(ZipLinks, Filenames),
             L3,
             table_tail(),
             dir_footer(Ctx, FullPath),%server:address(),
             doc_tail()
            ],

    B = lists:flatten(Body),

    Status = {200, "OK"},
    H = ewgi_api:response_headers(Ctx),
    CTHeader = {"Content-type", "text/html"},
    ewgi_api:response_headers([CTHeader|H],
			      ewgi_api:response_status(Status,
						       ewgi_api:response_message_body(B, Ctx))).

parse_query(Ctx) ->
    case ewgi_api:query_string(Ctx) of
        [PosC, $=, DirC] = Q ->
            Pos = case PosC of
                      $N -> 1; % name
                      $M -> 2; % last modified
                      $S -> 3; % size
                      $D -> 4  % Description                                            
                  end,
            Dir = case DirC of
                      $r -> reverse;
                      _  -> normal
                  end,
            {Pos, Dir, Q};
        _ ->
            {1, normal, ""}
    end.

parse_description(Line) ->
    L = string:strip(Line),
    Pos = string:chr(L,$ ),                                      
    Filename = string:substr(L, 1, Pos-1),
    D = string:substr(L,Pos+1),
    Description = string:strip(D,left),
    {Filename,Description}.

read_descriptions(DirName) ->
    File = DirName ++ [$/ | "MANIFEST.txt"],
    case file:read_file(File) of
        {ok,Bin} -> Lines = string:tokens(binary_to_list(Bin),"\n"),
                    lists:map(fun parse_description/1,Lines);
        _ -> []
    end.

get_description(Name,Descriptions) ->
    case lists:keysearch(Name,1,Descriptions) of
        {value, {_,Description}} -> Description;
        _ -> []
    end.

doc_head(DirName) ->
    HtmlDirName = yaws_api:htmlize(yaws_api:url_decode(DirName)),
    ["<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>\n"
     "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\n"       
     "<html>\n"
     " <head>\n"
     "  <title>Index of ", HtmlDirName,"</title>\n"
     "  <style type=\"text/css\">\n"
     "    img { border: 0; padding: 0 2px; vertical-align: text-bottom; }\n"
     "    td  { font-family: monospace; padding: 2px 3px; text-align:left;\n"
     "          vertical-align: bottom; white-space: pre; }\n"
     "    td:first-child { text-align: left; padding: 2px 10px 2px 3px; }\n"
     "    table { border: 0; }\n"
     "  </style>\n"
     "</head> \n"
     "<body>\n"].

doc_tail() ->
    "</body>\n"
	"</html>\n".

table_head(GetPath, Direction) ->
    NextDirection = if Direction == normal  -> "r";
                       Direction == reverse -> "n"
                    end,
    ["<table>\n"
     "  <tr>\n"
     "    <td><img src=\"",GetPath,"?icons/blank.gif\" alt=\"&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;\"/><a href=\"?N=",NextDirection,"\">Name</a></td>\n"
     "    <td><a href=\"?M=",NextDirection,"\">Last Modified</a></td>\n"
     "    <td><a href=\"?S=",NextDirection,"\">Size</a></td>\n"
     "    <td><a href=\"?D=",NextDirection,"\">Description</a></td>\n"
     "  </tr>\n"
     "  <tr><th colspan=\"4\"><hr/></th></tr>\n"].

table_tail() ->
    "  <tr><th colspan=\"4\"><hr/></th></tr>\n"
	"</table>\n".


dir_footer(Ctx, DirName) ->
    File = DirName ++ [$/ | "README.txt"],
    case file:read_file(File) of
        {ok,Bin} -> "<pre>\n" ++ binary_to_list(Bin) ++ "</pre>\n";
        _ -> address(Ctx)
    end.

dir_header(DirName, GetPath) ->
    File = DirName ++ [$/ | "HEADER.txt"],
    case file:read_file(File) of
        {ok,Bin} -> "<pre>\n" ++ binary_to_list(Bin) ++ "</pre>\n";
        _ ->     HtmlDirName = yaws_api:htmlize(yaws_api:url_decode(GetPath)),
                 "<h1>Index of " ++ HtmlDirName ++ "</h1>\n"
    end.

parent_dir() ->    
    {Gif, Alt} = list_gif(directory,"."),    
    ImgSrc = "?icons/" ++ Gif,
    ["  <tr>\n"
     "    <td><img src=\"", ImgSrc, "\" alt=\"", Alt, "\"/><a href=\"..\">Parent Directory</a></td>\n"
     "    <td></td>\n"
     "    <td>-</td>\n"
     "    <td></td>\n"
     "  </tr>\n"].

address(Ctx) ->
    [ewgi_api:server_software(Ctx), " Server at ",
     ewgi_api:server_name(Ctx)|
     case ewgi_api:server_port(Ctx) of
	 undefined -> [];
	 Port -> [$:|Port]
     end]. 

do_zip_links(_, []) -> [];
do_zip_links(ZipLinks, _) ->
	zip_links(ZipLinks, []).

zip_links([], Acc) ->
    lists:reverse(Acc);
zip_links([zip|R], Acc) ->
    zip_links(R, [allzip()|Acc]);
zip_links([tgz|R], Acc) ->
    zip_links(R, [alltgz()|Acc]);
zip_links([tbz2|R], Acc) ->
    zip_links(R, [alltbz2()|Acc]);
zip_links([_|R], Acc) ->
    zip_links(R, Acc);
%% On invalid input
zip_links(_, _) -> [].

%% FIXME: would be nice with a good size approx.  but it would require
%% a deep scan of possibly the entire docroot, (and also some knowledge
%% about zip's compression ratio in advance...)
allzip() ->
    {Gif, Alt} = list_gif(zip,""),    
    ImgSrc = "?icons/" ++ Gif,    
    ["  <tr>\n"
     "    <td><img src=\"", ImgSrc, "\" alt=\"", Alt, "\"/><a href=\"?all.zip\">all.zip</a>"
     "    <td></td>\n"
     "    <td>-</td>\n"
     "    <td>Build a zip archive of current directory</td>\n"
     "  </tr>\n"].

alltgz() ->
    {Gif, Alt} = list_gif(zip,""),
    ImgSrc = "?icons/" ++ Gif,
    ["  <tr>\n"
     "    <td><img src=\"", ImgSrc, "\" alt=\"", Alt, "\"/><a href=\"?all.tgz\">all.tgz</a>"
     "    <td></td>\n"
     "    <td>-</td>\n"
     "    <td>Build a gzip archive of current directory</td>\n"
     "  </tr>\n"].

alltbz2() ->
    {Gif, Alt} = list_gif(zip,""),
    ImgSrc = "?icons/" ++ Gif,    
    ["  <tr>\n"
     "    <td><img src=\"", ImgSrc, "\" alt=\"", Alt, "\"/><a href=\"?all.tbz2\">all.tbz2</a>"
     "    </td>\n"
     "    <td></td>\n"
     "    <td>-</td>\n"
     "    <td>Build a bzip2 archive of current directory</td>\n"
     "  </tr>\n"].


file_entry({ok, FI}, GetPath, Name, Qry, Descriptions) ->
    Ext = filename:extension(Name),
    {Gif, Alt} = list_gif(FI#file_info.type, Ext),
    QryStr = if FI#file_info.type == directory -> Qry;
                true -> ""
             end,

    Description = get_description(Name,Descriptions),

    HRef = GetPath ++ yaws_api:url_encode(Name) ++ QryStr,

    ImgSrc = "?icons/" ++ Gif,
    Entry = 
        ["  <tr>\n"
	 "    <td>"
	 "<img src=\"", ImgSrc, "\" alt=\"", Alt, "\"/>"
	 "<a href=\"", HRef, "\" title=\"", Name, "\">", trim(Name,?FILE_LEN_SZ), "</a>"
	 "</td>\n"
	 "    <td>", datestr(FI), "</td>\n"
	 "    <td>", sizestr(FI), "</td>\n"
	 "    <td>", Description, "</td>\n"
	 "  </tr>\n"],

    {true, {Name, FI#file_info.mtime, FI#file_info.size, Description, Entry}};

file_entry(_Err, _, _Name, _, _) ->
    false.

trim(L,N) ->
    trim(L,N,[]).
trim([_H1,_H2,_H3]=[H|T], 3=I, Acc) ->
    trim(T, I-1, [H|Acc]);
trim([_H1,_H2,_H3|_T], 3=_I, Acc) ->
    lists:reverse(Acc) ++ "..&gt;";
trim([H|T], I, Acc) ->
    trim(T, I-1, [H|Acc]);
trim([], _I, Acc) ->
    lists:reverse(Acc).

%% FI -> 16-Jan-2006 23:06
datestr(FI) ->    
    {{Year, Month, Day}, {Hour, Min, _}} = FI#file_info.mtime,
    io_lib:format("~s-~s-~w ~s:~s",
                  [yaws:mk2(Day),yaws:month(Month),Year,
                   yaws:mk2(Hour),yaws:mk2(Min)]).

sizestr(FI) when FI#file_info.size > 1000000 ->
    ?F("~.1fM", [FI#file_info.size / 1000000]);
sizestr(FI) when FI#file_info.size > 1000 ->
    ?F("~wk", [trunc(FI#file_info.size / 1000)]);
sizestr(FI) when FI#file_info.size == 0 ->
    ?F("0k", []);
sizestr(_FI) ->
    ?F("1k", []). % As apache does it...

list_gif(directory, ".") ->
    {"back.gif", "[DIR]"};
list_gif(regular, ".txt") -> 
    {"text.gif", "[TXT]"};
list_gif(regular, ".c") ->
    {"c.gif", "[&nbsp;&nbsp;&nbsp;]"};
list_gif(regular, ".dvi") ->
    {"dvi.gif", "[&nbsp;&nbsp;&nbsp;]"};
list_gif(regular, ".pdf") ->
    {"pdf.gif", "[&nbsp;&nbsp;&nbsp;]"};
list_gif(regular, _) ->
    {"layout.gif", "[&nbsp;&nbsp;&nbsp;]"};
list_gif(directory, _) ->
    {"dir.gif", "[DIR]"};
list_gif(zip, _) ->
    {"compressed.gif", "[DIR]"};
list_gif(_, _) ->
    {"unknown.gif", "[OTH]"}.

serve_icon(Ctx, Icon) ->
	case code:where_is_file(?MODULE_STRING ++ ".beam") of
		non_existing ->
			ewgi_api:response_status({404, "Not Found"},
				ewgi_api:response_message_body("Not Found", Ctx));
		Dir ->
			Filename =
				filename:dirname(Dir) ++
				"/../src/middleware/" ++
				?MODULE_STRING ++ "/icons/" ++Icon,
			ewgi_stream_file:run(Ctx, [Filename])			
	end.

