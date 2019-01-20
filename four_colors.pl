neighbours(austria        , [czech_republic, germany, hungary, italy, slovenia, slovakia]).
neighbours(belgium        , [france, netherlands, luxemburg, germany, united_kingdom]).
neighbours(bulgaria       , [romania, greece]).
neighbours(croatia        , [slovenia, hungary]).
neighbours(cyprus         , [greece]).
neighbours(czech_republic , [germany, poland, slovakia, austria]).
neighbours(denmark        , [germany, sweden]).
neighbours(estonia        , [finland, latvia, lithuania]).
neighbours(finland        , [estonia, sweden]).
neighbours(france         , [spain, belgium, luxemburg, germany, italy, united_kingdom]).
neighbours(germany        , [netherlands, belgium, luxemburg, denmark, france, austria, poland, czech_republic]).
neighbours(greece         , [bulgaria, cyprus]).
neighbours(hungary        , [austria, slovakia, romania, croatia, slovenia]).
neighbours(ireland        , [united_kingdom]).
neighbours(italy          , [france, austria, slovenia]).
neighbours(latvia         , [estonia, lithuania]).
neighbours(luxemburg      , [belgium, france, germany]).
neighbours(malta          , []).
neighbours(netherlands    , [belgium, germany, united_kingdom]).
neighbours(poland         , [germany, czech_republic, slovakia, lithuania]).
neighbours(portugal       , [spain]).
neighbours(romania        , [hungary, bulgaria]).
neighbours(slovakia       , [czech_republic, poland, hungary, austria]).
neighbours(slovenia       , [austria, italy, hungary, croatia]).
neighbours(spain          , [france, portugal]).
neighbours(sweden         , [finland, denmark]).
neighbours(united_kingdom , [ireland, netherlands, belgium, france]).

colour_countries(Colours) :-
  setof(Country/_, X^neighbours(Country,X), Colours),
  colours(Colours).

colours([]).

colours([Country/Colour | Rest]):-
  colours(Rest),
  member(Colour, [green, yellow, red, purple]),
  \+ (member(CountryA/Colour, Rest), neighbours(Country, CountryA)).

neighbours(Country, CountryA):-
  neig hbours(Country, Neighbours),
  member(CountryA, Neighbours).

member(X, [X|_]).
member(X, [_|Tail]):-
  member(X, Tail).

% Let us now execute the program by invoking colour_countries/1.
% ?- colour_countries(Map).
%
% Example response:
% Map = [
%   austria/yellow,
%   belgium/purple, bulgaria/yellow,
%   croatia/yellow, cyprus/yellow, czech_republic/purple,
%   denmark/yellow,
%   estonia/red,
%   finland/yellow, france/yellow,
%   germany/red, greece/green,
%   hungary/red,
%   ireland/yellow, italy/red,
%   latvia/green, luxemburg/green,
%   malta/green,
%   netherlands/yellow,
%   poland/yellow, portugal/yellow,
%   romania/green, slovakia/green, slovenia/green, spain/green, sweden/green,
%   united_kingdom/green
%   ].