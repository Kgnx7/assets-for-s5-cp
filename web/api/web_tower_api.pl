:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).

:- ['../../tower.pl'].

% URL handlers.
:- http_handler('/', handle_request, []).

tower(_{count:N}, _{answer: P}):-
  move(N, "1", "2", "3", P).

solve(_{predicat:X, props:Y}, N) :-
  atom_string(P, X),
  call(P, Y, N).

handle_request(Request) :-
    http_read_json_dict(Request, Query),
    solve(Query, Solution),
    reply_json_dict(Solution).

server(Port) :-
    http_server(http_dispatch, [port(Port)]).

:- initialization(server(8000)).






