:- begin_tests(abbreviated_dates_tests).  % for plunit
:- use_module(facts/country_language).
:- use_module(facts/country_date_endianness).

% Dates hinting week day name & month name

test('Lowercased full week day, comma, day, month name'):-
  solutions([date(2021, 9, 21)], 'saturday, 23 april', Dates, Languages, Formats),
  assertion(Dates == [date(2022, 4, 23)]),
  assertion(Languages == ['English']),
  assertion(Formats == ['%A, %d %B']).

test('Capitalized full Week Day, comma, Dot Postfixed Day & Full Month Name'):-
  solutions([date(2021, 9, 21)], 'Friday, 7. May', Dates, Languages, Formats),
  assertion(Dates == [date(2027,5,7)]),
  assertion(Languages == ['English']),
  assertion(Formats == ['%A, %d %B']).


% Dates hinting week day names

test('day_of_the_week_as_abbreviation'):-
  solutions([date(2022, 9, 7)], 'Di. 13.9.', Dates, Languages, Formats),
  assertion(Dates == [date(2022,9,13),date(2026,9,13)]),
  assertion(Languages == ['Dutch','French','German']),
  assertion(Formats == ['%a %d %m']).

test('day_of_the_week_as_consonant_abbreviation'):-
  solutions([date(2022, 9, 7)], 'pt. 16.09', Dates, Languages, Formats),
  assertion(Dates == [date(2022, 9, 16)]),
  assertion(Languages == ['Croatian', 'Polish', 'Slovak', 'Slovenian']),
  assertion(Formats == ['%a %d %m']).

test('day_of_the_week_as_single_consonant_abbreviation'):-
  solutions([date(2022, 9, 7)], 'T 13.9', Dates, Languages, Formats),
  assertion(Dates == [
    date(2022,9,13),date(2023,9,13),date(2024,9,13),date(2025,9,13),
    date(2027,9,13),date(2028,9,13)]),
  assertion(Languages == [
    'Croatian','Danish','English','Estonian','Finnish','Latvian','Lithuanian',
    'Portuguese','Slovak','Slovenian','Swedish','Vietnamese'
  ]),
  assertion(Formats == ['%a %d %m']).

test('day_of_the_week_as_abbreviation'):-
  solutions([date(2022, 9, 7)], '09-17, št', Dates, Languages, Formats),
  assertion(Dates == [date(2022, 9, 17)]),
  assertion(Languages == ['Lithuanian']),
  assertion(Formats == ['%m %d %a']).
  
test('day_of_the_week_as_abbreviation'):-
  solutions([date(2022, 9, 7)], '09-15, kt', Dates, Languages, Formats),
  assertion(Dates == [date(2022, 9, 15)]),
  assertion(Languages == ['Lithuanian']),
  assertion(Formats == ['%m %d %a']).

test('day_of_the_week_as_abbreviation'):-
  solutions([date(2022, 9, 7)], 'čt 15. 9.', Dates, Languages, Formats),
  assertion(Dates == [date(2022, 9, 15)]),
  assertion(Languages == ['Croatian','Czech','Slovenian']),
  assertion(Formats == ['%a %d %m']).
         
test('day_of_the_week_as_abbreviation'):-
  solutions([date(2022, 9, 7)], 'st 14. 9.', Dates, Languages, Formats),
  assertion(Dates == [date(2022,9,14),date(2024,9,14)]),
  assertion(Languages == ['Czech','English','Latvian','Slovak']),
  assertion(Formats == ['%a %d %m']).

test('day_of_the_week_as_abbreviation_with_comma'):-
  solutions([date(2022, 9, 14)], 'sri, 21. 09.', Dates, Languages, Formats),
  assertion(Dates == [date(2022,9,21)]),
  assertion(Languages == ['Croatian']),
  assertion(Formats == ['%a %d %m']).
  
test('Capitalized Abbreviated Month Name, Dash Deparated Day & Zero prefixed Month Number'):-
  solutions([date(2022, 2, 28)], 'Pirm. 20-06', Dates, Languages, Formats),
  assertion(Dates == [date(2022, 6, 20)]),
  assertion(Languages == ['Latvian','Lithuanian']),
  assertion(Formats == ['%a %d %m']).

test('Capitalized Abbreviated Month Name, Dash Deparated Zero prefixed Month Number & Day'):-
  solutions([date(2022, 2, 28)], 'Pirm. 06-20', Dates, Languages, Formats),
  assertion(Dates == [date(2022, 6, 20)]),
  assertion(Languages == ['Lithuanian']),
  assertion(Formats == ['%a %m %d']).

test('Capitalized full week day, day'):-
  solutions([date(2022, 2, 28)], 'Saturday, 2', Dates, Languages, Formats),
  assertion(Dates == [date(2022,4,2),date(2022,7,2),date(2023,9,2),date(2023,12,2)]),
  assertion(Languages == ['English']),
  assertion(Formats == ['%A %d']).

test('Capitalized full week day, day, dot, month number, dot'):-
  solutions([date(2022, 2, 28)], 'Petak 24.06.', Dates, Languages, Formats),
  assertion(Dates == [date(2022, 6, 24)]),
  assertion(Languages == ['Croatian']),
  assertion(Formats == ['%A %d %m']).

test('Capitalized full week day, day,dot, month number, dot'):-
  solutions([date(2022, 2, 28)], 'Ponedjeljak 20.06.', Dates, Languages, Formats),
  assertion(Dates == [date(2022, 6, 20)]),
  assertion(Languages == ['Croatian']),
  assertion(Formats == ['%A %d %m']).

% Dates hinting month names

test('Capitalized full Month Name & Day'):-
  solutions([date(2021, 9, 21)], 'July 4', Dates, Languages, Formats),
  assertion(Dates == [date(2022, 7, 4)]),
  assertion(Languages == ['English']),
  assertion(Formats == ['%B %d']).

test('Day & Capitalized full Month Name'):-
  solutions([date(2021, 9, 21)], '4 July', Dates, Languages, Formats),
  assertion(Dates == [date(2022, 7, 4)]),
  assertion(Languages == ['English']),
  assertion(Formats == ['%d %B']).

test('Day & Capitalized Explicitly Abbreviated Month Name'):-
  solutions([date(2021, 9, 21)], '23 Sep.', Dates, Languages, Formats),
  assertion(Dates == [date(2021, 9, 23)]),
  assertion(Languages == [
    'Danish','Dutch','English','Estonian','French','German','Latvian',
    'Norwegian','Romanian','Slovak','Slovenian','Spanish','Swedish']),
  assertion(Formats == ['%d %b']).

% Dates hinting just days

test('Absolute Day'):-
  solutions([date(2021, 9, 21)], '4', Dates, Languages, Formats),
  assertion(Dates == [date(2021, 10, 4)]),
  assertion(Languages == [
    'Bulgarian','Chinese','Croatian','Czech','Danish','Dutch','English',
    'Estonian','Finnish','French','German','Greek','Hebrew','Hungarian',
    'Italian','Japanese','Latvian','Lithuanian','Norwegian','Polish',
    'Portuguese','Romanian','Russian','Slovak','Slovenian','Spanish','Swedish',
    'Turkish','Ukrainian','Vietnamese']),
  assertion(Formats == ['%d']).

% Dates hinting relative days

test('Today'):-
  solutions([date(2021, 9, 21)], 'Today', Dates, Languages, Formats),
  assertion(Dates == [date(2021, 9, 21)]),
  assertion(Languages == ['English']),
  assertion(Formats == ['today']).

test('Tomorrow'):-
  solutions([date(2021, 9, 21)], 'Tomorrow', Dates, Languages, Formats),
  assertion(Dates == [date(2021, 9, 22)]),
  assertion(Languages == ['English']),
  assertion(Formats == ['tomorrow']).

%
% Solution wrapper
%
solutions(Context, Case, Dates, Languages, Formats):-
  atom_codes(Case, Codes),
  setof(Date, L^F^result(Context, Codes, Date, L, F), Dates),
  setof(Language, D^F^result(Context, Codes, D, Language, F), Languages),
  setof(Format, D^L^result(Context, Codes, D, L, Format), Formats).

result(Context, Case, Dates, Languages, Formats):-
  phrase(abbreviated_dates:single_day(Context, Dates, Languages, Formats), Case).

:- end_tests(abbreviated_dates_tests).
