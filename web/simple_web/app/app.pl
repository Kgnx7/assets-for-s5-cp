:- use_module(library(sw/simple_web)).
:- use_module(library(http/http_parameters)).

:- ["../../../four_colors.pl"].
:- ["../../../tower.pl"].
:- ["../../../queens.pl"].
:- ["../../../knights_and_knaves.pl"].

sw:route(home, '/', _Request) :-
    Resp = data{ 
        title: 'Предикаты'
    },
    reply_template(menu, Resp, options{cache:true}).

sw:route(four_colors, '/fourColors', _Request) :-
    colour_countries(Map),
    Resp = data{ 
        title: 'Four colors',
        result: Map
    },
    reply_template(temp, Resp, options{cache:true}).

sw:route(tower, '/tower', method(get), Request) :-
    http_parameters(Request, [number_of_tower(Number, [integer])]),
    move(Number, '1', '2', '3', P),
    Resp = data{
        title: 'Tower',
        result: P
    },
    reply_template(temp, Resp, options{cache:true}).

sw:route(queens, '/queens', method(get), Request) :-
    http_parameters(Request, [number_of_queens(Number, [integer])]),
    queens(Number, Qs),
    Resp = data{
        title: 'Queens',
        result: Qs
    },
    reply_template(temp, Resp, options{cache:true}).

sw:route(knights, '/knights', method(get), Request) :-
    http_parameters(Request, [scenario(Number, [integer])]),
    example_knights(Number, Knights),
    Resp = data{
        title: 'Knights',
        result: Knights
    },
    reply_template(temp, Resp, options{cache:true}).

:- run([port(5000)]).

