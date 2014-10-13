:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/websocket)).
:- use_module(library(http/http_server_files)).


%% Paths/routes
:- http_handler(root(.),	http_reply_file('index.html', []), []).
:- http_handler(root(move),	move, []).
:- http_handler(css('main.css'),  http_reply_file('css/main.css', []), []).
:- http_handler(js('main.js'), http_reply_file('js/main.js', []), []).
:- http_handler(root(ws), http_upgrade_to_websocket(websocket_handler, []), [spawn([])]).


http:location(css, root(css), []).
http:location(js, root(js), []).


%% At the moment a simple echo
websocket_handler(WebSocket) :-
    ws_receive(WebSocket, Message),
    (Message.opcode == close
    ->  true
    ;   ws_send(WebSocket, Message),
     websocket_handler(WebSocket)
    ).



%% Sets up the server
serve:-
    http_server(http_dispatch, [port(9000)]).

:- serve.
