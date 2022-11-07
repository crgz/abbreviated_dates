:- module(test_epigrapher, [test_epigrapher/0]).
:- use_module(epigrapher).
:- use_module(library(date_time)).

test_epigrapher :- run_tests.

:- begin_tests(epigrapher).

test('parse_parameters'):-
  parse_parameters(_{from:'2022-02-28',grapheme:'grapheme'}, Dates, Grapheme),
  assertion(Dates == date(2022, 2, 28)),
  assertion(Grapheme == 'grapheme').

test('Ignore Bad Date'):-
  date_get(today, Good),
  parse_time_fail_safe('Bad', Date),
  assertion(Date == Good).

:- end_tests(epigrapher).

