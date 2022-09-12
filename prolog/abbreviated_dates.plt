:- begin_tests(abbreviated_dates_tests).  % for plunit
:- use_module(facts/country_language).
:- use_module(facts/country_date_endianness).


case(Case, Date, Languages, Format):-
  phrase(abbreviated_dates:single_day([date(2022, 9, 7)], Date, Languages, Format), Case).


% Dates hinting week day name & month name

test('Lowercased full week day, comma, day, month name', all(Syntax == ['%A, %d %B'])) :-
  phrase(abbreviated_dates:single_day([date(2021, 9, 21)], date(2022, 4, 23), 'English', Syntax), `saturday, 23 april`).

test('Capitalized full Week Day, comma, Dot Postfixed Day & Full Month Name',
  fixme(all(Syntax == ['%A, %d %B','%A, %d %b'])))
:-
  phrase(abbreviated_dates:single_day([date(2021, 9, 21)], date(2022, 5, 7), 'English', Syntax), `Friday, 7. May`).

% Dates hinting week day names

test('day_of_the_week_as_abbreviation'):-
  solutions([date(2022, 9, 7)], `Di. 13.9.`, Dates, Languages, Formats),
  assertion(Dates == [date(2022, 9, 13)]),
  assertion(Languages == ['Dutch','German']),
  assertion(Formats == ['%a %d %m']).

test('day_of_the_week_as_consonant_abbreviation'):-
  solutions([date(2022, 9, 7)], `pt. 16.09`, Dates, Languages, Formats),
  assertion(Dates == [date(2022, 9, 16)]),
  assertion(Languages == ['Croatian', 'Polish', 'Slovak', 'Slovenian']),
  assertion(Formats == ['%a %d %m']).

test('day_of_the_week_as_single_consonant_abbreviation'):-
  solutions([date(2022, 9, 7)], `T 13.9`, Dates, Languages, Formats),
  assertion(Dates == [date(2022,9,13),date(2023,9,13),date(2024,9,13)]),
  assertion(Languages == [
    'Croatian', 'Danish', 'English', 'Estonian', 'Finnish', 'Latvian',
    'Lithuanian', 'Portuguese', 'Slovak', 'Slovenian', 'Swedish', 'Vietnamese'
  ]),
  assertion(Formats == ['%a %d %m']).

test('day_of_the_week_as_abbreviation'):-
  solutions([date(2022, 9, 7)], `09-17, št`, Dates, Languages, Formats),
  assertion(Dates == [date(2022, 9, 17)]),
  assertion(Languages == ['Lithuanian']),
  assertion(Formats == ['%m %d %a']).
  
test('day_of_the_week_as_abbreviation'):-
  solutions([date(2022, 9, 7)], `09-15, kt`, Dates, Languages, Formats),
  assertion(Dates == [date(2022, 9, 15)]),
  assertion(Languages == ['Lithuanian']),
  assertion(Formats == ['%m %d %a']).

test('day_of_the_week_as_abbreviation'):-
  solutions([date(2022, 9, 7)], `čt 15. 9.`, Dates, Languages, Formats),
  assertion(Dates == [date(2022, 9, 15)]),
  assertion(Languages == ['Croatian','Czech','Slovenian']),
  assertion(Formats == ['%a %d %m']).
         
test('day_of_the_week_as_abbreviation'):-
  solutions([date(2022, 9, 7)], `st 14. 9.`, Dates, Languages, Formats),
  assertion(Dates == [date(2022,9,14),date(2024,9,14)]),
  assertion(Languages == ['Czech','English','Latvian','Slovak']),
  assertion(Formats == ['%a %d %m']).
  
test('Capitalized Abbreviated Month Name, Dash Deparated Day & Zero prefixed Month Number',
  all(Languages == ['Latvian','Lithuanian']), all( Format==['%A %m %d']))
:-
  phrase(abbreviated_dates:single_day([date(2022, 2, 28)], date(2022, 6, 20), Languages, Format), `Pirm. 20-06`).

test('Capitalized Abbreviated Month Name, Dash Deparated Zero prefixed Month Number & Day',
  all(Languages == ['Lithuanian']), all( Format==['%A %m %d']))
:- phrase(abbreviated_dates:single_day([date(2022, 2, 28)], date(2022, 6, 20), Languages, Format), `Pirm. 06-20`).

test('Capitalized full week day, day ', all(Date==[date(2023, 9, 2),date(2023, 12, 2),date(2024, 3, 2)])) :-
  phrase(abbreviated_dates:single_day([date(2022, 9, 7)], Date, 'English', '%A %d'), `Saturday, 2`).

test('Capitalized full week day, day, dot, month number, dot', fixme(all(Syntax == ['%A %d %m','%a %d %m']))) :-
  phrase(abbreviated_dates:single_day([date(2022, 2, 28)], Date, 'Croatian', Syntax), `Petak 24.06.`),
  assertion(Date == date(2022, 6, 24)).

test('Capitalized full week day, day,dot, month number, dot', fixme(all(Syntax == ['%A %d %m','%a %d %m']))) :-
  phrase(abbreviated_dates:single_day([date(2022, 2, 28)], Date, 'Croatian', Syntax), `Ponedjeljak 20.06.`),
  assertion(Date == date(2022, 6, 20)).

% Dates hinting wonth names

test('Capitalized full Month Name & Day', fixme(all(Syntax == ['%B %d','%b %d']))) :-
  phrase(abbreviated_dates:single_day([date(2021, 9, 21)], date(2022, 7, 4), 'English', Syntax), `July 4`).

test('Day & Capitalized full Month Name', fixme(all(Syntax == ['%d %B','%d %b']))) :-
  phrase(abbreviated_dates:single_day([date(2021, 9, 21)], date(2022, 7, 4), 'English', Syntax), `4 July`).

test('Day & Capitalized Explicitly Abbreviated Month Name', all(Syntax == ['%d %b'])) :-
  phrase(abbreviated_dates:single_day([date(2020, 2, 28)], date(2020, 9, 23), 'English', Syntax), `23 Sep.`).

% Dates hinting just days
test('Absolute Day', all(Syntax == ['%d'])) :-
  phrase(abbreviated_dates:single_day([date(2021, 9, 21)], date(2021, 10, 4), 'English', Syntax), `4`).

% Dates hinting relative days
test('Today', all(Syntax == [today])) :-
  phrase(abbreviated_dates:single_day([date(2021, 9, 21)], date(2021, 9, 21), 'English', Syntax), `Today`).

test('Tomorrow', all(Syntax == [tomorrow])) :-
  phrase(abbreviated_dates:single_day([date(2020, 2, 29)], date(2020, 3, 1), 'English', Syntax), `Tomorrow`).


solutions(Context, Case, Dates, Languages, Formats):-
  setof(Date, L^F^result(Context, Case, Date, L, F), Dates),
  setof(Language, D^F^result(Context, Case, D, Language, F), Languages),
  setof(Format, D^L^result(Context, Case, D, L, Format), Formats).

result(Context, Case, Dates, Languages, Formats):-
  phrase(abbreviated_dates:single_day(Context, Dates, Languages, Formats), Case).

:- end_tests(abbreviated_dates_tests).
