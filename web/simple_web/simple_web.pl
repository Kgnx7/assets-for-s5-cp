:- module(hello_world, [serve/0]).

:- use_module(library(sw/simple_web)).

sw:route(home, '/', _Request) :-
	reply_html("<h1>Hello, world!</h1>").

serve :-
	run(port(8000)).


