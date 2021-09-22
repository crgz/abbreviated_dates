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

% Use Cases

parse(date(2021, 9, 21), 'Freitag, 7. Mai', Dates, Trace) ->
    Dates = [date(2022, 5, 7)],
    Trace = [sd(wd('German'), dm(explicit('German')))].

parse(date(2021, 9, 21), '23 Sze. - 27 Sze.', Dates, Trace) ->
    Dates = [date(2021, 9, 23), date(2021, 9, 27)],
    Trace = [dm(abbreviated('Hungarian')), dm(abbreviated('Hungarian'))] .