%%%----------------------------------------------------------------------
%%% File    : ewgi_websocket
%%% Author  : Davide Marquês <nesrait@gmail.com>
%%% Purpose : 
%%% Created :  December 2009 by Davide Marquês <nesrait@gmail.com>
%%%----------------------------------------------------------------------
-module(ewgi_websocket).
-author('nesrait@gmail.com').

%% Ewgi Application API
-export([run/2]).

%% Usage example
-export([websocket_example/1]).

%% The body field is used to specify which process will be owning the websocket.
run(Ctx, [WebSocketOwner]) when is_pid(WebSocketOwner) ->
	WS = {websocket, WebSocketOwner},
	ewgi_api:response_message_body(WS, Ctx).


%%--------------------------------------------------------------------
%% Websocket example
%%--------------------------------------------------------------------
websocket_example(Ctx) ->
	case ewgi_api:get_header_value("Upgrade", Ctx) of
		undefined ->
			serve_html_page(Ctx);
		"WebSocket" ->
			WebSocketOwner = spawn(fun() -> websocket_owner(Ctx) end),
			?MODULE:run(Ctx, [WebSocketOwner])
	end.


websocket_owner(Ctx) ->
	SocketMode = false, %% passive mode
	case ewgi_api:websocket_init(Ctx, SocketMode) of
	{ok, WebSocket} ->
		%% This is how we read messages (plural!!) from websockets on passive mode
		{ok, Messages} = ewgi_api:websocket_receive(WebSocket),
		case Messages of
			[<<"client-connected">>] ->
				ewgi_api:websocket_setopts(WebSocket, [{active, true}]),
				echo_server(WebSocket);
			Other ->
				io:format("websocket_owner got: ~p. Terminating~n", [Other])
		end;
	_ -> ok
	end.


echo_server(WebSocket) ->
	receive
		{tcp, WebSocket, DataFrame} ->
			Data = ewgi_api:websocket_unframe_data(DataFrame),
			io:format("Got data from Websocket: ~p~n", [Data]),
            ewgi_api:websocket_send(WebSocket, Data), 
            echo_server(WebSocket);
		{tcp_closed, WebSocket} ->
			io:format("Websocket closed. Terminating echo_server...~n");
		Any ->
			io:format("echo_server received msg:~p~n", [Any]),
			echo_server(WebSocket)
	end.


serve_html_page(Ctx) ->
	Hostname = ewgi_api:server_name(Ctx),
	PortStr = case ewgi_api:server_port(Ctx) of
							undefined -> "";
							P -> ":" ++ P
						end,
	Path = ewgi_api:path_info(Ctx),
	WebSocketLocation = Hostname ++ PortStr ++ Path,
	Body = html_body(WebSocketLocation),
	Status = {200, "Ok"},
	H = ewgi_api:response_headers(Ctx),
	CTHeader = {"Content-type", "text/html"},
	ewgi_api:response_headers([CTHeader|H],
		ewgi_api:response_status(Status,
			ewgi_api:response_message_body(Body, Ctx))).

%% this html was copied from the basic example in
%% http://github.com/davebryson/erlang_websocket/
html_body(WebSocketLocation) ->
"<html>
<head> 
  <title>Basic WebSocket Example</title> 
  <script type=\"text/javascript\"> 
     if (!window.WebSocket)
        alert(\"WebSocket not supported by this browser\");
    
    // Get an Element
    function $() { return document.getElementById(arguments[0]); }
    // Get the value of an Element
    function $F() { return document.getElementById(arguments[0]).value; }
    
    var client = {
        connect: function(){
         this._ws=new WebSocket(\"ws://" ++ WebSocketLocation ++ "\");
         this._ws.onopen=this._onopen;
         this._ws.onmessage=this._onmessage;
         this._ws.onclose=this._onclose;
        },
        _onopen: function(){
          $('connect').className='hidden';
          $('connected').className='';
          $('phrase').focus();
          client._send('client-connected');
       },
        _send: function(message){
           if (this._ws)
            this._ws.send(message);
        },
       chat: function(text) {
          if (text != null && text.length>0 )
            client._send(text);
        },
        _onmessage: function(m) {
          if (m.data){
            var text = m.data; 
            var msg=$('msgs');
            var spanText = document.createElement('span');
            spanText.className='text';
            spanText.innerHTML=text;
            var lineBreak = document.createElement('br');
            msg.appendChild(spanText);
            msg.appendChild(lineBreak);
            msg.scrollTop = msg.scrollHeight - msg.clientHeight;   
          }
        },
        _onclose: function(m) {
          this._ws=null;
          $('connect').className='';
          $('connected').className='hidden';
          $('msg').innerHTML='';
        }
    };
  </script> 
  <style type='text/css'> 
    div.hidden { display: none; }
  </style> 
 
</head> 
<body> 
  <h1>Basic Echo Example</h1> 
  <div id=\"msgs\"></div> 
  <div id=\"connect\"> 
     <input id='cA' class='button' type='submit' name='connect' value='Connect'/> 
  </div> 
  <br/> 
  <div id=\"connected\" class=\"hidden\"> 
    Say Something:&nbsp;<input id='phrase' type='text'/> 
    <input id='sendB' class='button' type='submit' name='connect' value='Send'/> 
  </div> 
 
  <script type='text/javascript'> 
    $('cA').onclick = function(event) { client.connect(); return false; };
    $('sendB').onclick = function(event) { client.chat($F('phrase')); $('phrase').value=''; return false; };	
 </script> 
  </body> 
</html>".

