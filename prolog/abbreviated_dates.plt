:- begin_tests(abbreviated_dates_tests).  % for plunit
:- use_module(facts/country_language).
:- use_module(facts/country_date_endianness).
%
% Pirm is an abbreviation of a week day name in Latvian and Lithuanian
% The following tests describes how desambiguation can be solved by different facts.

test('Pirm. 20-06', all(Languages == ['Latvian','Lithuanian'])) :- % Explanation:
  assertion(top_endianness('Latvia', little)), % Latvia little date endianness and first number month day
  assertion(top_country_language('Latvia', 'Lithuanian')),  % Lithuanian speaking population in Latvia
  phrase(abbreviated_dates:single_day([date(2022, 2, 28)], date(2022, 6, 20), Languages, '%a %m %d'), `Pirm. 20-06`).

test('Pirm. 06-20', all(Languages == ['Lithuanian'])) :-
  assertion(top_endianness('Lithuania', big)),
  % Since date endianness for Lithuania is big the second number should be the day
  phrase(abbreviated_dates:single_day([date(2022, 2, 28)], date(2022, 6, 20), Languages, '%a %m %d'), `Pirm. 06-20`).

%
%  Initial tests defined using tap
%
test('4 July', fixme(all(Syntax == ['%d %B','%d %b']))) :-
  phrase(abbreviated_dates:single_day([date(2021, 9, 21)], date(2022, 7, 4), 'English', Syntax), `4 July`).

test('July 4', fixme(all(Syntax == ['%B %d','%b %d']))) :-
  phrase(abbreviated_dates:single_day([date(2021, 9, 21)], date(2022, 7, 4), 'English', Syntax), `July 4`).

test('23 Sep.', all(Syntax == ['%d %b'])) :-
  phrase(abbreviated_dates:single_day([date(2020, 2, 28)], date(2020, 9, 23), 'English', Syntax), `23 Sep.`).

test('4', all(Syntax == ['%d'])) :-
  phrase(abbreviated_dates:single_day([date(2021, 9, 21)], date(2021, 10, 4), 'English', Syntax), `4`).

test('Saturday, 2', all(Syntax == ['%A, %d'])) :-
  phrase(abbreviated_dates:single_day([date(2020, 2, 28)], date(2020, 3, 2), 'English', Syntax), `Saturday, 2`).

test('Petak 24.06.', fixme(all(Syntax == ['%A %d %m','%a %d %m']))) :-
  phrase(abbreviated_dates:single_day([date(2022, 2, 28)], Date, 'Croatian', Syntax), `Petak 24.06.`),
  assertion(Date == date(2022, 6, 24)).

test('Friday, 7. May', fixme(all(Syntax == ['%A, %d %B','%A, %d %b']))) :-
  phrase(abbreviated_dates:single_day([date(2021, 9, 21)], date(2022, 5, 7), 'English', Syntax), `Friday, 7. May`).

test('saturday, 23 april', all(Syntax == ['%A, %d %B'])) :-
  phrase(abbreviated_dates:single_day([date(2021, 9, 21)], date(2022, 4, 23), 'English', Syntax), `saturday, 23 april`).

test('Ponedjeljak 20.06.', fixme(all(Syntax == ['%A %d %m','%a %d %m']))) :-
  phrase(abbreviated_dates:single_day([date(2022, 2, 28)], Date, 'Croatian', Syntax), `Ponedjeljak 20.06.`),
  assertion(Date == date(2022, 6, 20)).

test('Today', all(Syntax == [today])) :-
  phrase(abbreviated_dates:single_day([date(2021, 9, 21)], date(2021, 9, 21), 'English', Syntax), `Today`).

test('Tomorrow', all(Syntax == [tomorrow])) :-
  phrase(abbreviated_dates:single_day([date(2020, 2, 29)], date(2020, 3, 1), 'English', Syntax), `Tomorrow`).

:- end_tests(abbreviated_dates_tests).
