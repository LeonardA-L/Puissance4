:- use_module(library(http/websocket)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).

:- http_handler(root(ws),
                http_upgrade_to_websocket(echo, []),
                [spawn([])]).

echo(WebSocket) :-
    ws_receive(WebSocket, Message),
    (Message.opcode == close
    ->  true
    ;   ws_send(WebSocket, Message),
        echo(WebSocket)
    ).