:- begin_tests(abbreviated_dates_tests).  % for plunit
:- use_module(facts/country_language).
:- use_module(facts/country_date_endianness).
%
% Pirm is an abbreviation of a week day name in Latvian and Lithuanian
% The following tests describes how desambiguation can be solved by different facts.

test('Pirm. 20-06', [all(Languages == ['Latvian','Lithuanian'])]) :- % Explanation:
  assertion(top_endianness('Latvia', little)), % Latvia little date endianness and first number month day
  assertion(top_country_language('Latvia', 'Lithuanian')),  % Lithuanian speaking population in Latvia
  phrase(abbreviated_dates:single_day([date(2022, 2, 28)], date(2022, 6, 20), Languages, '%a %m %d'), `Pirm. 20-06`).

test('Pirm. 06-20', [all(Languages == ['Lithuanian'])]) :-
  assertion(top_endianness('Lithuania', big)),
  % Since date endianness for Lithuania is big the second number should be the day
  phrase(abbreviated_dates:single_day([date(2022, 2, 28)], date(2022, 6, 20), Languages, '%a %m %d'), `Pirm. 06-20`).

:- end_tests(abbreviated_dates_tests).
