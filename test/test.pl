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

parse(date(2021, 9, 21), '4 July', Dates, Syntax, Language) ->
    Dates = [date(2022, 7, 4)],
    Syntax = ['%d %B'],
    Language = 'English'.

parse(date(2021, 9, 21), 'July 4', Dates, Syntax, Language) ->
   Dates = [date(2022, 7, 4)],
   Syntax = ['%B %d'],
   Language = 'English'.

parse(date(2020, 2, 28), '23 Sep.', Dates, Syntax, Language) ->
   Dates = [date(2020, 9, 23)],
   Syntax = ['%d %b'],
   Language = 'English'.

parse(date(2021, 9, 21), '4', Dates, Syntax, Language) ->
   Dates = [date(2021, 10, 4)],
   Syntax = ['%d'],
   Language = 'English'.

parse(date(2020, 2, 28), 'Saturday, 2', Dates, Syntax, Language) ->
   Dates = [date(2020, 3, 2)],
   Syntax = ['%A, %d'],
   Language = 'English'.

parse(date(2022, 2, 28), 'ma 13/6', Dates, Syntax, Language) ->
   Dates = [date(2022, 6, 13)],
   Syntax = ['%a %d %m'],
   Language = 'Danish'.

parse(date(2022, 2, 28), 'Petak 24.06.', Dates, Syntax, Language) ->
   Dates = [date(2022, 6, 24)],
   Syntax = ['%A %d %m'],
   Language = 'Croatian'.

parse(date(2021, 9, 21), 'Friday, 7. May', Dates, Syntax, Language) ->
   Dates = [date(2022, 5, 7)],
   Syntax = ['%A, %d %B'],
   Language = 'English'.

parse(date(2021, 9, 21), 'saturday, 23 april', Dates, Syntax, Language) ->
   Dates = [date(2022, 4, 23)],
   Syntax = ['%A, %d %B'],
   Language = 'English'.

parse(date(2022, 2, 28), 'Ponedjeljak 20.06. - Petak 24.06.', Dates, Syntax, Language) ->
   Dates = [date(2022, 6, 20), date(2022, 6, 24)],
   Syntax = ['%A %d %m', '%A %d %m'],
   Language = 'Croatian'.

parse(date(2021, 9, 21), 'Today', Dates, Syntax, Language) ->
   Dates = [date(2021, 9, 21)],
   Syntax = [today],
   Language = 'English'.

parse(date(2020, 2, 29), 'Tomorrow', Dates, Syntax, Language) ->
   Dates = [date(2020, 3, 1)],
   Syntax = [tomorrow],
   Language = 'English'.
