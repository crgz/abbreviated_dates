/*
Author: Conrado M. Rodriguez <Conrado.Rgz@gmail.com> https://github.com/crgz
*/
:- module(abbreviated_dates,
[
  parse/3,   % +Context, +Expression, ?Dates
  parse/4,   % +Context, +Expression, ?Dates, ?Syntax
  parse/5    % +Context, +Expression, ?Dates, ?Syntax, ?Language
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
  parse(Context, Expression, Dates, _, _).

parse(Context, Expression, Dates, Syntax) :-
  parse(Context, Expression, Dates, Syntax, _).

parse(Context, Expression, Dates, Syntax, Language) :-
  atom_codes(Expression, Codes), phrase(multiple_days([Context], Dates, Language, Syntax), Codes).

%-----------------------------------------------------------
% Grammar
%
multiple_days(_, [], _, []) --> [].
multiple_days([LastKnownDate|Other], [SingleDay|MultipleDays], Language, [S1|S2]) -->
  single_day([LastKnownDate|Other], SingleDay, Language, S1),
  (" - " | eos),
  multiple_days([SingleDay, LastKnownDate|Other], MultipleDays, Language, S2).

% Dates hinting week day name & month name

% phrase(abbreviated_dates:single_day([date(2020, 2, 28)], Date, Language, Syntax), `Wednesday, 1 July`).
single_day([Context|_], date(Year,Month,Day), Language, Syntax) -->
  string(WeekDayCodes), ",", b, date_number(Day), b, month(Month, Language, MonthFormat),
  {
    possible_year(Context, Year),
    week_day_facts(WeekDayCodes, WeekDayNumber, Language, WeekDaySyntax),
    week_dayn(date(Year,Month,Day), WeekDayNumber),
    atomic_list_concat([WeekDaySyntax, ', %d ', MonthFormat], Syntax)
  }.

% Dates hinting week day names

% phrase(abbreviated_dates:single_day([date(2022, 2, 28)], Date, Language, Syntax), `Pirm. 06-20`).
single_day([Context|_], date(Y, M, D), Language, Syntax) -->
  week_day(WeekDayCodes), optional_comma, b, date_number(First), separator, date_number(Second),
  {
    week_day_facts(WeekDayCodes, WeekDayNumber, Language, WeekDaySyntax),
    best_date(Context, First, Second, WeekDayNumber, Language, Y, D, M, DayMonthSyntax),
    atomic_list_concat([WeekDaySyntax, DayMonthSyntax], ' ', Syntax)
  }.

% phrase(abbreviated_dates:single_day([date(2022, 2, 28)], Date, Language, Syntax), `06-20, Pirm.`).
single_day([Context|_], date(Y, M, D), Language, Syntax) -->
  date_number(First), separator, date_number(Second), optional_comma, b, week_day(WeekDayCodes),
  {
    week_day_facts(WeekDayCodes, WeekDayNumber, Language, WeekDaySyntax),
    best_date(Context, First, Second, WeekDayNumber, Language, Y, D, M, DayMonthSyntax),
    atomic_list_concat([DayMonthSyntax, WeekDaySyntax], ' ', Syntax)
  }.

% Dates hinting wonth names

% phrase(abbreviated_dates:single_day([date(2020, 2, 28)], Date, Language, Syntax), `1 July`).
single_day([Context|_], date(Y, M, D), Language, Syntax) -->
  date_number(D), b, month(M, Language, MonthFormat), 
  {maybe_future_year(Context, M, D, Y), atom_concat('%d ', MonthFormat, Syntax)}.

% phrase(abbreviated_dates:single_day([date(2020, 2, 28)], Date, Language, Syntax), `Jan. 1`).
single_day([Context|_], date(Y, M, D), Language, Syntax) -->
  month(M, Language, MonthFormat), b, date_number(D), 
  {maybe_future_year(Context, M, D, Y), atom_concat(MonthFormat,' %d', Syntax)}.

% Dates hinting just days

% phrase(abbreviated_dates:single_day([date(2020, 2, 28)], Date, Language, Syntax), `31`).
single_day([Context|_], Date, Language, '%d') --> 
  date_number(D), 
  {future_date(Context, D, Date), language(Language)}.

% Dates hinting relative days

% phrase(abbreviated_dates:single_day([date(2020, 2, 29)], Date, Language, Syntax), `Tomorrow`).
single_day([Context|_], Date, Language, Syntax) -->
  nonblanks(Codes),
  {atom_codes(Adverb, Codes), adverb(Language, Adverb, Context, Date, Syntax)}.


month(MonthNumber, Language, '%B') --> % explicit month
  nonblanks(Codes),
  {
    atom_codes(InputMonthName, Codes), downcase_atom(InputMonthName, LowerCaseMonthName),
    month_name(Language, MonthNumber , KnownMonthName), downcase_atom(KnownMonthName, LowerCaseMonthName)
  }.

month(MonthNumber, Language, '%b') --> % abbreviated month
  string(Abbreviation), optional_period,
  { 
    atom_codes(Prefix, Abbreviation),
    month_name(Language, MonthNumber, MonthName),
    sub_atom(MonthName, 0, _, _, Prefix)
  }.

week_day(InputCodes) --> % abbreviated week day
  string_without(". ", InputCodes), optional_period.

week_day_facts(InputCodes, WeekDayNumber, Language, Format):-
  atom_codes(InputAtom, InputCodes),
  downcase_atom(InputAtom, LowerCaseInputAtom),
  week_day_name(Language, WeekDayNumber, WeekDayName),
  downcase_atom(WeekDayName, LowerCaseWeekDayName),
  optional_abbreviation(LowerCaseWeekDayName, LowerCaseInputAtom, Abbreviated),
  select_abbreviation_format(Abbreviated, Format).

date_number(N) --> integer(N).
date_number(N) --> integer(N), ".".
separator --> "/"; "-"; "."; " ".
b --> white.
optional_period --> "."; "".
optional_comma --> ","; "".

%-----------------------------------------------------------
% Internal predicates
%

best_date(Context, First, Second, WeekDayNumber, Language, Year, Day, Month, Syntax):-
  possible_year(Context, Year),
  top_country_language(Country, Language),
  top_endianness(Country, Endianness),
  day_month_order(Endianness, First, Second, Day, Month),
  week_dayn(date(Year,Month,Day), WeekDayNumber),
  day_month_syntax(Syntax, First, Second, Day, Month).

possible_year(Context, Year):-
  date_extract(Context, years(Y)),
  Max is Y + 10,
  between(Y, Max, Year).

day_month_order(little, Day,   Month, Day, Month). % day is first number in little endian dates
day_month_order(middle, Month, Day,   Day, Month). % day is second number in little middle dates
day_month_order(big,    Month, Day,   Day, Month). % day is second number in big middle dates

day_month_syntax('%d %m', Day, Month, Day, Month).
day_month_syntax('%m %d', Month, Day, Day, Month).

% Find optional abbreviations ordering by length
optional_abbreviation(Atom, Abbreviation, Abbreviated):-
  order_by([desc(L)], (sub_atom(Atom, 0, _, After, Abbreviation), atom_length(Abbreviation,L))),
  L > 0,
  (After = 0 -> Abbreviated = false; Abbreviated = true).

select_abbreviation_format(true, '%a').
select_abbreviation_format(false, '%A').

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

maybe_future_year(Context, Month, Day, Year) :-
  date_extract(Context, years(Y)),
  ( date_compare(date(Y, Month, Day), >=, Context) -> Year = Y; Year is Y + 1).

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
