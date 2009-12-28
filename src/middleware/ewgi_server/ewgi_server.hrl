
-record(sconf, {
		docroot="priv/www",
		docroot_mountpoint="/",
		dir_content_listing=true,
		index_files = ["index.yaws", "index.php", "index.cgi", "index.html"],
		appmods = [
			{"/hello",
					{ewgi_examples_hello, run, []}
				},
			{"/gimme_the_file",
					{ewgi_stream_file, run, ["priv/www/test.txt"]}
				}
		],
		extension_mods = [
				{"txt",
					{ewgi_server_extension_mods, logged_download, []}},
				{php,
					{ewgi_server_extension_mods, fastcgi, [
							{app_server_host, "127.0.0.1"},
							{app_server_port, 3000},
							{trace_protocol, false},
							{log_app_error, false}
						]}},
				{php_, {ewgi_server_extension_mods, interpreted_cgi,
							"d:/Programas/xampplite/php/php-cgi.exe"}},
				{cgi, {ewgi_server_extension_mods, cgi, []}}
				%% {"yaws", {yaws_compiler_thingy, ...?}}
			]
	}).


-record(urltype, {type,   %% error | yaws | regular | directory | 
                          %% forbidden | appmod 
                  finfo,
                  fullpath = [], %% deep list (WHY?)
                  dir = [],      %% relative dir where the path leads to
                                 %% flat | unflat need flat for authentication
                  data,          %% type-specific e.g: Binary | FileDescriptor 
                                 %% | DirListing | undefined
                  deflate,       %% undefined | Binary | dynamic
                  mime = "text/html",    %% MIME type
                  scriptname = [],
                  pathinfo = []
                 }).


-ifdef(debug).
-define(Debug(F, A),
	io:format("{debug, ~p, ~p, ~n   " ++ F ++ "}", [?FILE,?LINE| A])).
-else.
-define(Debug(_F, _A), ok).
-endif.