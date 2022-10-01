/*
Author: Conrado M. Rodriguez <Conrado.Rgz@gmail.com> https://github.com/crgz
*/
:- module(abbreviated_dates,
[
  parse/3,      % +Context, +Expression, ?Dates
  parse/4,      % +Context, +Expression, ?Dates, ?Syntax
  parse/5,      % +Context, +Expression, ?Dates, ?Syntax, ?Language
  parse/6,      % +Context, +Expression, ?Dates, ?Syntax, ?Language, ?Country
  single_day/7  % Esported for testing purpose. (See: abbreviated_dates.plt)
]).

:- [library(dcg/basics)].
:- use_module(library(date_time)).
:- use_module(facts/languages).  % Facts about languages
:- use_module(facts/country_language).
:- use_module(facts/country_date_endianness).

%!  parse(+Context, +Expression, ?Dates)
%
%   True if Dates can be parsed from Expression and Dates is greater than the Reference date.
%
%   ==
%   parse(date(29,02,2020), 'saturday, 23 april', Dates, Syntax, Language).
%   ==

parse(Context, Expression, Dates) :-
  parse(Context, Expression, Dates, _, _, _).

parse(Context, Expression, Dates, Syntax) :-
  parse(Context, Expression, Dates, Syntax, _, _).

parse(Context, Expression, Dates, Syntax, Language) :-
  parse(Context, Expression, Dates, Syntax, Language, _).

parse(Context, Expression, Dates, Syntax, Language, Country) :-
  atom_codes(Expression, Codes), phrase(multiple_days([Context], Dates, Language, Syntax, Country), Codes).

%-----------------------------------------------------------
% Grammar
%
multiple_days(_, [], _, []) --> [].
multiple_days([LastKnownDate|Other], [SingleDay|MultipleDays], Language, [S1|S2], Country) -->
  single_day([LastKnownDate|Other], SingleDay, Language, S1, Country),
  (" - " | eos),
  multiple_days([SingleDay, LastKnownDate|Other], MultipleDays, Language, S2).

% DATES HINTING WEEKDAY NAME, DAY NUMBER AND MONTH NAME

% phrase(abbreviated_dates:single_day([date(2020, 2, 28)], Date, Language, Syntax), `Wednesday, 1 July`).
single_day([Context|_], date(Year,MonthNumber,Day), Language, Syntax, _) -->
  string_without(",", WeekDay), ",", b, date_number(Day), b, string(Month),
  {
    factor_month(Month, implicit, Language, MonthNumber, MonthFormat),
    factor_week_day(WeekDay, WeekDayNumber, Language, WeekDaySyntax),
    possible_year(Context, Year),
    week_dayn(date(Year,MonthNumber,Day), WeekDayNumber),
    date_compare(date(Year,MonthNumber,Day), >=, Context),
    atomic_list_concat([WeekDaySyntax, ', %d ', MonthFormat], Syntax)
  }.

% DATES HINTING WEEKDAY NAMES AND TWO NUMBERS

% phrase(abbreviated_dates:single_day([date(2022, 2, 28)], Date, Language, Syntax), `Pirm. 06-20`).
single_day([Context|_], Date, Language, Syntax, Country) --> % Explicit abbreviation
  string(WeekDayCodes), ".", b, date_number(First), separator, date_number(Second),
  {
    solve_date_numbers(Context, WeekDayCodes, First, Second, Date, Language, DayMonthSyntax, WeekDaySyntax, Country),
    atomic_list_concat([WeekDaySyntax, DayMonthSyntax], ' ', Syntax)
  }.
% phrase(abbreviated_dates:single_day([date(2022, 2, 28)], Date, Language, Syntax), `Pirm, 06-20`).
single_day([Context|_], Date, Language, Syntax, Country) -->
  string(WeekDayCodes), ",", b, date_number(First), separator, date_number(Second),
  {
    solve_date_numbers(Context, WeekDayCodes, First, Second, Date, Language, DayMonthSyntax, WeekDaySyntax, Country),
    atomic_list_concat([WeekDaySyntax, DayMonthSyntax], ' ', Syntax)
  }.
% phrase(abbreviated_dates:single_day([date(2022, 2, 28)], Date, Language, Syntax), `Pirm 06-20`).
single_day([Context|_], Date, Language, Syntax, Country) -->
  string(WeekDayCodes), b, date_number(First), separator, date_number(Second),
  {
    solve_date_numbers(Context, WeekDayCodes, First, Second, Date, Language, DayMonthSyntax, WeekDaySyntax, Country),
    atomic_list_concat([WeekDaySyntax, DayMonthSyntax], ' ', Syntax)
  }.
% phrase(abbreviated_dates:single_day([date(2022, 2, 28)], Date, Language, Syntax), `Th., 06-20`).
single_day([Context|_], Date, Language, Syntax, Country) -->
  string(WeekDayCodes), ".", ",", b, integer(First), "-", integer(Second),
  {
    solve_date_numbers(Context, WeekDayCodes, First, Second, Date, Language, DayMonthSyntax, WeekDaySyntax, Country),
    atomic_list_concat([WeekDaySyntax, '., ', DayMonthSyntax], Syntax)
  }.

% phrase(abbreviated_dates:single_day([date(2022, 2, 28)], Date, Language, Syntax), `Th., 16.06.`).
single_day([Context|_], Date, Language, Syntax, Country) -->
  string(WeekDayCodes), ".", ",", b, integer(First), ".", integer(Second), ".",
  {
    solve_date_numbers(Context, WeekDayCodes, First, Second, Date, Language, DayMonthSyntax, WeekDaySyntax, Country),
    atomic_list_concat([WeekDaySyntax, '., ', DayMonthSyntax], Syntax)
  }.

% phrase(abbreviated_dates:single_day([date(2022, 2, 28)], Date, Language, Syntax), `Th., 16.06`).
single_day([Context|_], Date, Language, Syntax, Country) -->
  string(WeekDayCodes), ".", ",", b, integer(First), ".", integer(Second),
  {
    solve_date_numbers(Context, WeekDayCodes, First, Second, Date, Language, DayMonthSyntax, WeekDaySyntax, Country),
    atomic_list_concat([WeekDaySyntax, '., ', DayMonthSyntax], Syntax)
  }.

% DATES HINTING WEEKDAY NAMES AND ONE NUMBER

% phrase(abbreviated_dates:single_day([date(2022, 2, 28)], Date, Language, Syntax), `saturday, 23`).
single_day([Context|_], Date, Language, Syntax, _) -->
  string(WeekDayCodes), ",", b, month_day(Day),
  {
    factor_week_day(WeekDayCodes, WeekDayNumber, Language, WeekDaySyntax),
    possible_day(Context, Day, Date),
    week_dayn(Date, WeekDayNumber),
    atomic_list_concat([WeekDaySyntax, ' %d'], Syntax)
  }.

% DATES HINTING TWO NUMBERS AND WEEKDAY NAMES

% phrase(abbreviated_dates:single_day([date(2022, 2, 28)], Date, Language, Syntax), `06-20, Pirm`).
single_day([Context|_], Date, Language, Syntax, Country) -->
  date_number(First), separator, date_number(Second), ",", b, string(WeekDayCodes),
  {
    solve_date_numbers(Context, WeekDayCodes, First, Second, Date, Language, DayMonthSyntax, WeekDaySyntax, Country),
    atomic_list_concat([DayMonthSyntax, WeekDaySyntax], ' ', Syntax)
  }.

% DATES HINTING ONE NUMBER AND A MONTH NAME

% phrase(abbreviated_dates:single_day([date(2020, 2, 28)], Date, Language, Syntax), `4 July`).
single_day([Context|_], Date, Language, Syntax, _) -->
  month_day(Day), b, string(Month),
  {factor_month_day(Context, Day, Month, implicit, Date, Language, MonthFormat),atom_concat('%d ', MonthFormat, Syntax)}.

% phrase(abbreviated_dates:single_day([date(2020, 2, 28)], Date, Language, Syntax), `23 Sep.`).
single_day([Context|_], Date, Language, Syntax, _) --> % Explicit abbreviation
  month_day(Day), b, string(Month), ".", 
  {factor_month_day(Context, Day, Month, explicit, Date, Language, MonthFormat),atom_concat('%d ', MonthFormat, Syntax)}.

% phrase(abbreviated_dates:single_day([date(2020, 2, 28)], Date, Language, Syntax), `23. Sep`).
single_day([Context|_], Date, Language, Syntax, _) --> % Explicit abbreviation
  month_day(Day), ".", b, string(Month), 
  {factor_month_day(Context, Day, Month, implicit, Date, Language, MonthFormat),atom_concat('%d ', MonthFormat, Syntax)}.

% phrase(abbreviated_dates:single_day([date(2020, 2, 28)], Date, Language, Syntax), `23. Sep.`).
single_day([Context|_], Date, Language, Syntax, _) --> % Explicit abbreviation
  month_day(Day), ".", b, string(Month), ".", 
  {factor_month_day(Context, Day, Month, implicit, Date, Language, MonthFormat),atom_concat('%d ', MonthFormat, Syntax)}.

% DATES HINTING A MONTH NAME AND ONE NUMBER 

% phrase(abbreviated_dates:single_day([date(2020, 2, 28)], Date, Language, Syntax), `Jan. 1`).
single_day([Context|_], Date, Language, Syntax, _) -->
  string(Month), b, month_day(Day),
  {factor_month_day(Context, Day, Month, implicit, Date, Language, MonthFormat), atom_concat(MonthFormat,' %d', Syntax)}.

% DATES HINTING JUST DAYS

% phrase(abbreviated_dates:single_day([date(2020, 2, 28)], Date, Language, Syntax), `31`).
single_day([Context|_], Date, Language, '%d', _) -->
  month_day(D),
  {future_date(Context, D, Date), language(Language)}.

% DATES HINTING RELATIVE DAYS

% phrase(abbreviated_dates:single_day([date(2020, 2, 29)], Date, Language, Syntax), `Tomorrow`).
single_day([Context|_], Date, Language, Syntax, _) -->
  nonblanks(Codes),
  {atom_codes(Adverb, Codes), adverb(Language, Adverb, Context, Date, Syntax)}.


month_day(Day) --> integer(Day), {between(1, 31, Day)}.
date_number(N) --> integer(N).
date_number(N) --> integer(N), ".".
separator --> "-"|"."|" ".
b --> white.

%-----------------------------------------------------------
% Grammar supporting predicates
%

solve_date_numbers(Context, WeekDayCodes, First, Second, Date, Language, DayMonthSyntax, WeekDaySyntax, Country):-
  factor_week_day(WeekDayCodes, WeekDayNumber, Language, WeekDaySyntax),
  possible_year(Context, Year),
  factor_country_endianness(Language, First, Second, date(Year,Month,Day), DayMonthSyntax, Country),
  week_dayn(date(Year,Month,Day), WeekDayNumber),
  Date = date(Year,Month,Day).

factor_month_day(Context, Day, Month, Style, date(Year,MonthNumber,Day), Language, MonthFormat):-
  factor_month(Month, Style, Language, MonthNumber, MonthFormat),
  possible_year(Context, Year),
  date_compare(date(Year,MonthNumber,Day), >=, Context).

factor_week_day(InputCodes, WeekDayNumber, Language, Format):-
  atom_codes(InputAtom, InputCodes),
  downcase_atom(InputAtom, LowerCaseInputAtom),
  week_day_name(Language, WeekDayNumber, WeekDayName),
  downcase_atom(WeekDayName, LowerCaseWeekDayName),
  abbreviation(LowerCaseWeekDayName, LowerCaseInputAtom, IsAbbreviated),
  (IsAbbreviated -> Format = '%a'; Format = '%A').

factor_month(Month, Style, Language, MonthNumber, Syntax):-
  month_name(Language, MonthNumber, MonthName),
  abbreviation(MonthName, Abbreviation, IsAbbreviated),
  atom_codes(MaybeAbbreviation, Month),
  capitalize_sentence(MaybeAbbreviation, Abbreviation),
  (IsAbbreviated-> MonthFormat = '%b'; MonthFormat = '%B'),
  (IsAbbreviated, Style = explicit -> Suffix = '.'; Suffix = ''),
  atomic_list_concat([MonthFormat, Suffix], Syntax).

factor_country_endianness(Language, First, Second, date(Year,Month,Day), Syntax, Country):-
  top_endianness(Country, Endianness),     % Find a country with defined endianness
  top_country_language(Country, Language), % Check if the language is spoken there
  day_month_order(Endianness, First, Second, Day, Month, Syntax),
  valid(date(Year,Month,Day)).

day_month_order(little, Day,   Month, Day, Month, '%d %m'). % day is first number in little endian dates
day_month_order(middle, Month, Day,   Day, Month, '%m %d'). % day is second number in little middle dates
day_month_order(big,    Month, Day,   Day, Month, '%m %d'). % day is second number in big middle dates

valid(date(Year,Month,Day)):- Month =< 12, date_month_days(Month,Year,MD), Day =< MD.

possible_year(Context, Year):-
  date_extract(Context, years(Y)),
  Max is Y + 6,
  between(Y, Max, Year).

possible_day(Context, Day, Date):-
  date_extract(Context, years(Year)),
  date_extract(Context, months(M)),
  Max is M + 24,
  between(M, Max, Month),
  future_date(date(Year,Month,Day), Date).

% Find optional abbreviations ordering by length
abbreviation(Atom, Abbreviation, IsAbbreviated):-
  abbreviation_all_letters(Atom, Abbreviation, IsAbbreviated);
  abbreviation_consonant(Atom, Abbreviation, IsAbbreviated).

abbreviation_all_letters(Atom, Abbreviation, IsAbbreviated):-
  order_by([desc(L)], (sub_atom(Atom, 0, _, After, Abbreviation), atom_length(Abbreviation,L))),
  L > 0,
  (After = 0 -> IsAbbreviated = false; IsAbbreviated = true).

abbreviation_consonant(Atom, Abbreviation, IsAbbreviated):-
  atom_remove_vowels(Atom, AtomConsonants),
  abbreviation_all_letters(AtomConsonants, Abbreviation, IsAbbreviated).

atom_remove_vowels(Atom, AtomConsonants):-
  atom_chars(Atom, Chars),
  subtract(Chars, [a, ą, e, ę, i, o, ó, u], Consonants),
  remove_duplicates(Consonants, Uniques),
  atom_chars(AtomConsonants, Uniques).

future_date(date(Y, M, D), Day, date(YY,MM,DD)):-
  date_month_days(M,Y,MD),
  D =< Day, Day =< MD, % Day in current month
  !,
  future_date(date(Y,M,Day), date(YY,MM,DD)).
future_date(date(Y, M, D), Day, date(YY,MM,DD)):-
  date_month_days(M,Y,MD),
  (Day =< D; MD =< Day),  % Day out of current month
  !,
  M2 is M + 1,
  future_date(date(Y,M2,Day), date(YY,MM,DD)).
future_date(date(Y, M, D), date(YY,MM,DD)):-
  M > 12,
  !,
  M2 is M - 12,
  Y2 is Y + 1,
  future_date(date(Y2,M2,D), date(YY,MM,DD)).
future_date(date(Y,M,D), date(Y,M,D)).

date_month_days(0,_,31).
date_month_days(1,_,31).
date_month_days(2,Y,29) :- date_leap_year(Y), !.
date_month_days(2,_,28).
date_month_days(3,_,31).
date_month_days(4,_,30).
date_month_days(5,_,31).
date_month_days(6,_,30).
date_month_days(7,_,31).
date_month_days(8,_,31).
date_month_days(9,_,30).
date_month_days(10,_,31).
date_month_days(11,_,30).
date_month_days(12,_,31).
date_month_days(13,_,31).

date_leap_year(Y) :-
   ( ( 0 =:= Y mod 100, 0 =:= Y mod 400 ) ;
     ( 0 =\= Y mod 100, 0 =:= Y mod 4 ) ).

%
% Utility predicates
%
remove_duplicates(List, Uniques):- remove_duplicates(List, Uniques, []).
remove_duplicates([], [], _).
remove_duplicates([Head|Tail], Result, Seen) :-
  (  member(Head, Seen)
  -> (Result = Uniques, NewSeen = Seen)
  ;  (Result = [Head|Uniques], NewSeen = [Head])
  ),
  remove_duplicates(Tail, Uniques, NewSeen).

capitalize_sentence(LowerCase, UpperCase):-
  atom_chars(LowerCase, [Lower|Tail]),
  char_type(Lower, to_lower(Upper)),
  atom_chars(UpperCase, [Upper|Tail]).
