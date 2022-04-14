:- use_module('../prolog/abbreviated_dates').

term_expansion(Query -> Result, (Head :- Test)) :-
   format(atom(Head), '~w -> ~w', [Query, Result]),
   Test = (
      call(Query),
      call(Result),
      !
   ),
   tap:register_test(Head).

:- use_module(library(tap)).

% Use Cases 	█



parse(date(2021, 9, 21), '4 July', Dates, Syntax) ->
    Dates = [date(2022, 7, 4)],
    Syntax = ['English',  [['%d', '%B']]].

parse(date(2021, 9, 21), 'July 4', Dates, Syntax) ->
   Dates = [date(2022, 7, 4)],
   Syntax = ['English', [['%B', '%d']]].

parse(date(2020, 2, 28), 'Saturday, 2', Dates, Syntax) ->
   Dates = [date(2020, 3, 2)],
   Syntax = ['English', [['%A', '%d']]].

parse(date(2021, 9, 21), 'Friday, 7. May', Dates, Syntax) ->
   Dates = [date(2022, 5, 7)],
   Syntax = ['English', [['%A', ['%d', '%B']]]].

parse(date(2020, 2, 28), '23 Sep.', Dates, Syntax) ->
   Dates = [date(2020, 9, 23)],
   Syntax = ['English', [['%d', '%b']]].

parse(date(2020, 2, 28), '23 Sep. - 27 Sep.', Dates, Syntax) ->
   Dates = [date(2020, 9, 23), date(2020, 9, 27)],
   Syntax = ['English', [['%d', '%b'], ['%d', '%b']]].

parse(date(2021, 9, 21), 'Today', Dates, Syntax) ->
   Dates = [date(2021, 9, 21)],
   Syntax = ['English', [adverb]].

parse(date(2020, 2, 29), 'Tomorrow', Dates, Syntax) ->
   Dates = [date(2020, 3, 1)],
   Syntax = ['English', [adverb]].

parse(date(2021, 9, 21), '4', Dates, Syntax) ->
   Dates = [date(2021, 10, 4)],
   Syntax = ['English', ['%d']].
