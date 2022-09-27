:- module(test_abbreviated_dates, [test_abbreviated_dates/0]).
:- use_module(abbreviated_dates).

test_abbreviated_dates :- run_tests.

solutions(Context, Case, Dates, Languages, Formats):-
  atom_codes(Case, Codes),
  setof(Date, L^F^result(Context, Codes, Date, L, F), Dates),
  Context = [Start|_],
  findall(Date, (member(Date, Dates),date_time:date_compare(Date, <, Start)),PastDates),
  assertion(PastDates == []),
  setof(Language, D^F^result(Context, Codes, D, Language, F), Languages),
  setof(Format, D^L^result(Context, Codes, D, L, Format), Formats).

result(Context, Case, Dates, Languages, Formats):-
  phrase(abbreviated_dates:single_day(Context, Dates, Languages, Formats), Case).


:- begin_tests(abbreviated_dates).

% DATES HINTING WEEKDAY NAME, DAY NUMBER AND MONTH NAME

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

test('Capitalized full Week Day, comma, Day & Implicitly Abbreviated Month Name'):-
  solutions([date(2021, 9, 21)], 'Friday, 17 Jun', Dates, Languages, Formats),
  assertion(Dates == [date(2022,6,17)]),
  assertion(Languages == ['English']),
  assertion(Formats == ['%A, %d %b']).

test('Capitalized full Week Day, comma, Dot Postfixed Day & Implicitly Abbreviated Month Name'):-
  solutions([date(2021, 9, 21)], 'Friday, 17. Jun', Dates, Languages, Formats),
  assertion(Dates == [date(2022,6,17)]),
  assertion(Languages == ['English']),
  assertion(Formats == ['%A, %d %b']).

% DATES HINTING WEEKDAY NAMES AND TWO NUMBERS

test('Capitalized explicitly abbreviated weekday name'):-
  solutions([date(2022, 9, 7)], 'Di. 13.9.', Dates, Languages, Formats),
  assertion(Dates == [date(2022,9,13),date(2026,9,13)]),
  assertion(Languages == ['Dutch','French','German']),
  assertion(Formats == ['%a %d %m']).

test('Capitalized explicitly abbreviated weekday name, Dash Separated Day & Zero prefixed Month Number'):-
  solutions([date(2022, 2, 28)], 'Pirm. 20-06', Dates, Languages, Formats),
  assertion(Dates == [date(2022, 6, 20)]),
  assertion(Languages == ['Latvian','Lithuanian']),
  assertion(Formats == ['%a %d %m']).

test('Capitalized explicitly abbreviated weekday name, Dash Separated Zero prefixed Month Number & Day'):-
  solutions([date(2022, 2, 28)], 'Pirm. 06-20', Dates, Languages, Formats),
  assertion(Dates == [date(2022, 6, 20)]),
  assertion(Languages == ['Lithuanian']),
  assertion(Formats == ['%a %m %d']).

test('Capitalized explicitly abbreviated weekday name, comma, Dash Separated Zero prefixed Numbers'):-
  solutions([date(2022, 2, 28)], 'Th., 06-20', Dates, Languages, Formats),
  assertion(Dates == [date(2022,6,20),date(2023,6,20),date(2024,6,20),date(2025,6,20),date(2026,6,20),date(2028,6,20)]),
  assertion(Languages == ['English','Vietnamese']),
  assertion(Formats == ['%a., %m %d']).

test('Capitalized explicitly abbreviated weekday name, comma, Dot suffixed Numbers'):-
  solutions([date(2022, 2, 28)], 'Th., 16.06.', Dates, Languages, Formats),
  assertion(Dates == [date(2022,6,16),date(2023,6,16),date(2025,6,16),date(2026,6,16),date(2027,6,16),date(2028,6,16)]),
  assertion(Languages == ['English','Vietnamese']),
  assertion(Formats == ['%a., %d %m']).

test('Capitalized explicitly abbreviated weekday name, comma, Dot suffixed Number, Zero prefixed Number'):-
  solutions([date(2022, 2, 28)], 'Th., 16.06', Dates, Languages, Formats),
  assertion(Dates == [date(2022,6,16),date(2023,6,16),date(2025,6,16),date(2026,6,16),date(2027,6,16),date(2028,6,16)]),
  assertion(Languages == ['English','Vietnamese']),
  assertion(Formats == ['%a., %d %m']).

test('Capitalized week day name as implicit single consonant abbreviation'):-
  solutions([date(2022, 9, 7)], 'T 13.9', Dates, Languages, Formats),
  assertion(Dates == [date(2022,9,13),date(2023,9,13),date(2024,9,13),date(2025,9,13),date(2027,9,13),date(2028,9,13)]),
  assertion(Languages == [
    'Croatian','Danish','English','Estonian','Finnish','Latvian','Lithuanian','Portuguese','Slovak','Slovenian','Swedish',
    'Vietnamese']),
  assertion(Formats == ['%a %d %m']).

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

test('Lowercase week day name as explicit consonant abbreviation'):-
  solutions([date(2022, 9, 7)], 'pt. 16.09', Dates, Languages, Formats),
  assertion(Dates == [date(2022, 9, 16)]),
  assertion(Languages == ['Croatian', 'Polish', 'Slovak', 'Slovenian']),
  assertion(Formats == ['%a %d %m']).

test('Lowercase week day name as explicit consonant abbreviation and space separated numbers'):-
  solutions([date(2022, 9, 7)], 'čt 15. 9.', Dates, Languages, Formats),
  assertion(Dates == [date(2022, 9, 15)]),
  assertion(Languages == ['Croatian','Czech','Slovenian']),
  assertion(Formats == ['%a %d %m']).
         
test('Lowercase week day name as explicit duplicated consonant abbreviation and space separated numbers'):-
  solutions([date(2022, 9, 7)], 'st 14. 9.', Dates, Languages, Formats),
  assertion(Dates == [date(2022,9,14),date(2024,9,14)]),
  assertion(Languages == ['Czech','English','Latvian','Slovak']),
  assertion(Formats == ['%a %d %m']).

test('Lowercase week day name as implicit vowel abbreviation with comma and space separated numbers'):-
  solutions([date(2022, 9, 14)], 'sri, 21. 09.', Dates, Languages, Formats),
  assertion(Dates == [date(2022,9,21)]),
  assertion(Languages == ['Croatian']),
  assertion(Formats == ['%a %d %m']).
  

% DATES HINTING TWO NUMBERS AND WEEKDAY NAMES

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

% DATES HINTING WEEKDAY NAMES AND ONE NUMBER

test('Capitalized full week day, day'):-
  solutions([date(2022, 2, 28)], 'Saturday, 2', Dates, Languages, Formats),
  assertion(Dates == [date(2022,4,2),date(2022,7,2),date(2023,9,2),date(2023,12,2)]),
  assertion(Languages == ['English']),
  assertion(Formats == ['%A %d']).


% DATES HINTING ONE NUMBER AND A MONTH NAME

test('Day & Capitalized full Month Name'):-
  solutions([date(2021, 9, 21)], '4 July', Dates, Languages, Formats),
  assertion(Dates == [date(2022,7,4),date(2023,7,4),date(2024,7,4),date(2025,7,4),date(2026,7,4),date(2027,7,4)]),
  assertion(Languages == ['English']),
  assertion(Formats == ['%d %B']).

test('Day & Capitalized Explicitly Abbreviated Month Name'):-
  solutions([date(2021, 9, 21)], '23 Sep.', Dates, Languages, Formats),
  assertion(Dates == 
    [date(2021,9,23),date(2022,9,23),date(2023,9,23),date(2024,9,23),date(2025,9,23),date(2026,9,23),date(2027,9,23)]),
  assertion(Languages == [
    'Danish','Dutch','English','Estonian','French','German','Latvian',
    'Norwegian','Romanian','Slovak','Slovenian','Spanish','Swedish']),
  assertion(Formats == ['%d %b.']).

test('Day & Capitalized Implicitly Abbreviated Month Name'):-
  solutions([date(2021, 9, 21)], '17 Jun', Dates, Languages, Formats),
  assertion(Dates == [date(2022,6,17),date(2023,6,17),date(2024,6,17),date(2025,6,17),date(2026,6,17),date(2027,6,17)]),
  assertion(Languages == ['Danish','Dutch','English','German','Norwegian','Portuguese','Slovenian','Spanish','Swedish']),
  assertion(Formats == ['%d %b']).

test('Day & Capitalized Implicitly Full Month Name'):-
  solutions([date(2021, 9, 21)], '17 June', Dates, Languages, Formats),
  assertion(Dates == [date(2022,6,17),date(2023,6,17),date(2024,6,17),date(2025,6,17),date(2026,6,17),date(2027,6,17)]),
  assertion(Languages == ['English']),
  assertion(Formats == ['%d %B']).

test('Day & Capitalized Implicitly Abbreviated Month Name'):-
  solutions([date(2021, 9, 21)], '17. Jun', Dates, Languages, Formats),
  assertion(Dates == [date(2022,6,17),date(2023,6,17),date(2024,6,17),date(2025,6,17),date(2026,6,17),date(2027,6,17)]),
  assertion(Languages == ['Danish','Dutch','English','German','Norwegian','Portuguese','Slovenian','Spanish','Swedish']),
  assertion(Formats == ['%d %b']).

test('Day & Capitalized Explicitly Abbreviated Month Name'):-
  solutions([date(2021, 9, 21)], '17. Jun.', Dates, Languages, Formats),
  assertion(Dates == [date(2022,6,17),date(2023,6,17),date(2024,6,17),date(2025,6,17),date(2026,6,17),date(2027,6,17)]),
  assertion(Languages == [ 'Danish','Dutch','English','German','Norwegian','Portuguese','Slovenian','Spanish','Swedish']),
  assertion(Formats == ['%d %b']).


% DATES HINTING MONTH NAMES AND ONE NUMBER

test('Capitalized full Month Name & Day'):-
  solutions([date(2021, 9, 21)], 'July 4', Dates, Languages, Formats),
  assertion(Dates == [date(2022,7,4),date(2023,7,4),date(2024,7,4),date(2025,7,4),date(2026,7,4),date(2027,7,4)]),
  assertion(Languages == ['English']),
  assertion(Formats == ['%B %d']).

test('Capitalized Implicitly Abbreviated Month Name & Day'):-
  solutions([date(2021, 9, 21)], 'Jun 17', Dates, Languages, Formats),
  assertion(Dates == [date(2022,6,17),date(2023,6,17),date(2024,6,17),date(2025,6,17),date(2026,6,17),date(2027,6,17)]),
  assertion(Languages == ['Danish','Dutch','English','German','Norwegian','Portuguese','Slovenian','Spanish','Swedish']),
  assertion(Formats == ['%b %d']).


% DATES HINTING JUST DAYS

test('Absolute Day'):-
  solutions([date(2021, 9, 21)], '4', Dates, Languages, Formats),
  assertion(Dates == [date(2021, 10, 4)]),
  assertion(Languages == [
    'Bulgarian','Chinese','Croatian','Czech','Danish','Dutch','English','Estonian','Finnish','French','German','Greek',
    'Hebrew','Hungarian','Italian','Japanese','Latvian','Lithuanian','Norwegian','Polish','Portuguese','Romanian','Russian',
    'Slovak','Slovenian','Spanish','Swedish','Turkish','Ukrainian','Vietnamese']),
  assertion(Formats == ['%d']).

% DATES HINTING RELATIVE DAYS

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

:- end_tests(abbreviated_dates).
