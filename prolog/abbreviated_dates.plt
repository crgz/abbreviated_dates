:- begin_tests(abbreviated_dates_tests).  % for plunit
:- use_module(facts/country_language).
:- use_module(facts/country_date_endianness).
%
% Pirm is an abbreviation of a week day name in Latvian and Lithuanian
% The following tests describes how desambiguation can be solved by different facts.

test('Capitalized Explicitly Abbreviated Month Name, Dash Deparated Day & Zero prefixed Month Number',
  all(Languages == ['Latvian','Lithuanian']), all( Format==['%A %m %d']))
:- % Explanation:
  assertion(top_endianness('Latvia', little)), % Latvia little date endianness and first number month day
  assertion(top_country_language('Latvia', 'Lithuanian')),  % Lithuanian speaking population in Latvia
  phrase(abbreviated_dates:single_day([date(2022, 2, 28)], date(2022, 6, 20), Languages, Format), `Pirm. 20-06`).

test('Capitalized Explicitly Abbreviated Month Name, Dash Deparated Zero prefixed Month Number & Day',
  all(Languages == ['Lithuanian']), all( Format==['%A %m %d']))
:-
  assertion(top_endianness('Lithuania', big)),
  % Since date endianness for Lithuania is big the second number should be the day
  phrase(abbreviated_dates:single_day([date(2022, 2, 28)], date(2022, 6, 20), Languages, Format), `Pirm. 06-20`).

%
%  Initial tests for grammar rules defined using tap

% Week day names
test('Capitalized full week day, day ', all(Syntax == ['%A, %d'])) :-
  phrase(abbreviated_dates:single_day([date(2020, 2, 28)], date(2020, 3, 2), 'English', Syntax), `Saturday, 2`).

test('Capitalized full week day, day, dot, month number, dot', fixme(all(Syntax == ['%A %d %m','%a %d %m']))) :-
  phrase(abbreviated_dates:single_day([date(2022, 2, 28)], Date, 'Croatian', Syntax), `Petak 24.06.`),
  assertion(Date == date(2022, 6, 24)).

test('Capitalized full week day, day,dot, month number, dot', fixme(all(Syntax == ['%A %d %m','%a %d %m']))) :-
  phrase(abbreviated_dates:single_day([date(2022, 2, 28)], Date, 'Croatian', Syntax), `Ponedjeljak 20.06.`),
  assertion(Date == date(2022, 6, 20)).

% Week day name & month name
test('Lowercased full week day, comma, day, month name', all(Syntax == ['%A, %d %B'])) :-
  phrase(abbreviated_dates:single_day([date(2021, 9, 21)], date(2022, 4, 23), 'English', Syntax), `saturday, 23 april`).

test('Capitalized full Week Day, comma, Dot Postfixed Day & Full Month Name',
  fixme(all(Syntax == ['%A, %d %B','%A, %d %b'])))
:-
  phrase(abbreviated_dates:single_day([date(2021, 9, 21)], date(2022, 5, 7), 'English', Syntax), `Friday, 7. May`).

% Month names
test('Capitalized full Month Name & Day', fixme(all(Syntax == ['%B %d','%b %d']))) :-
  phrase(abbreviated_dates:single_day([date(2021, 9, 21)], date(2022, 7, 4), 'English', Syntax), `July 4`).

test('Day & Capitalized full Month Name', fixme(all(Syntax == ['%d %B','%d %b']))) :-
  phrase(abbreviated_dates:single_day([date(2021, 9, 21)], date(2022, 7, 4), 'English', Syntax), `4 July`).

test('Day & Capitalized Explicitly Abbreviated Month Name', all(Syntax == ['%d %b'])) :-
  phrase(abbreviated_dates:single_day([date(2020, 2, 28)], date(2020, 9, 23), 'English', Syntax), `23 Sep.`).

%Days
test('Absolute Day', all(Syntax == ['%d'])) :-
  phrase(abbreviated_dates:single_day([date(2021, 9, 21)], date(2021, 10, 4), 'English', Syntax), `4`).

%Relative days
test('Today', all(Syntax == [today])) :-
  phrase(abbreviated_dates:single_day([date(2021, 9, 21)], date(2021, 9, 21), 'English', Syntax), `Today`).

test('Tomorrow', all(Syntax == [tomorrow])) :-
  phrase(abbreviated_dates:single_day([date(2020, 2, 29)], date(2020, 3, 1), 'English', Syntax), `Tomorrow`).

:- end_tests(abbreviated_dates_tests).
