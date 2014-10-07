:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_server_files)).

:- http_handler(root(.),	http_reply_file('index.html', []), []).
:- http_handler(root(move),	move, []).
:- http_handler(css('main.css'),  http_reply_file('css/main.css', []), []).
:- http_handler(js('main.js'), http_reply_file('js/main.js', []), []).


http:location(css, root(css), []).
http:location(js, root(js), []).

serve:-
    http_server(http_dispatch, [port(9000)]).
:- serve.
