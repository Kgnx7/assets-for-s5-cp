:- use_module(library(sw/simple_web)).

sw:route(home, '/', _Request) :-
    reply_html("<h1>Hello, world!</h1>
                <img src='static/images/logo.png' alt='logo'/>").

sw:route(templates_test, '/test', _Request) :-
    Data = data{ title: 'Hello'
               , items: [ item{ title: 'Item 1', content: 'content of item 1' }
                        , item{ title: 'Item 2', content: 'content of item 2' }
                        ]
               },
    reply_template('test', Data, options{cache:true}).

sw:route(termarized_test, '/termarized_test', _Request) :-
    reply_html([title('Termerized')], [h1('Termerized Example'), p('With some text')]).

sw:route(api, '/api', method(get), _Request) :-
    reply_json_dict(data{example: "Hello, world!"}).

:- run([port(5000)]).
