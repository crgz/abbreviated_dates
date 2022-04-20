%-----------------------------------------------------------
% Module definition
%

:- module(abbreviated_dates, [parse/3, parse/4, parse/5]).   % get a date

%-----------------------------------------------------------
% Native dependency requirements
%

:- [library(dcg/basics)].
:- use_module(library(date_time)).
:- use_module(library(dcg/basics), [whites//0, nonblanks//1, digits//1]).


:- [facts/languages]. % Facts about languages

% 
% date_time:date_get(today, Today), parse(Today, 'Freitag, 7. Mai', Dates, Syntax).
% 
% 
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
  (dash | eos),
  multiple_days([SingleDay, LastKnownDate|Other], MultipleDays, Language, S2).

% phrase(abbreviated_dates:single_day([date(2020, 2, 28)], Date, Language, Syntax), `1 July`).
single_day([Context|_], date(Y, M, D), Language, Syntax) -->
  day_number(D), b, month(M, Language, MonthFormat), 
  {maybe_future_year(Context, M, D, Y), atom_concat('%d ', MonthFormat, Syntax)}.

% phrase(abbreviated_dates:single_day([date(2020, 2, 28)], Date, Language, Syntax), `Jan. 1`).
single_day([Context|_], date(Y, M, D), Language, Syntax) -->
  month(M, Language, MonthFormat), b, day_number(D), 
  {maybe_future_year(Context, M, D, Y), atom_concat(MonthFormat,' %d', Syntax)}.

% phrase(abbreviated_dates:single_day([date(2020, 2, 28)], Date, Language, Syntax), `31`).
single_day([Context|_], Date, Language, '%d') --> 
  day_number(D), 
  {future_date(Context, D, Date), language(Language)}.

% phrase(abbreviated_dates:single_day([date(2020, 2, 29)], Date, Language, Syntax), `Tomorrow`).
single_day([Context|_], Date, Language, Syntax) -->
  nonblanks(Codes),
  {atom_codes(Adverb, Codes), adverb(Language, Adverb, Context, Date, Syntax)}.

% phrase(abbreviated_dates:single_day([date(2020, 2, 28)], Date, Language, Syntax), `Saturday, 1 July`).
single_day(Context, Date, Language, Syntax) -->
  string(Codes), ",", b, single_day(Context, Date, Language, DaySyntax),
  {
    atom_codes(InputWeekDay, Codes), downcase_atom(InputWeekDay, LowerCaseWeekDay),
    week_day(Language, _, KnownWeekDay), downcase_atom(KnownWeekDay, LowerCaseWeekDay),
    atom_concat('%A, ', DaySyntax, Syntax)
  }.

day_number(D) --> integer(D).
day_number(D) --> integer(D), ".".

month(MonthNumber, Language, '%B') --> % explicit month
  nonblanks(Codes),
  {
    atom_codes(InputMonthName, Codes), downcase_atom(InputMonthName, LowerCaseMonthName),
    month_name(Language, MonthNumber , KnownMonthName), downcase_atom(KnownMonthName, LowerCaseMonthName)
  }.

month(MonthNumber, Language, '%b') --> % abbreviated month
  string(Abbreviation), ".",
  { 
    atom_codes(Prefix, Abbreviation),
    month_name(Language, MonthNumber, MonthName),
    sub_atom(MonthName, 0, _, _, Prefix)
  }.

dash --> " - ".
b --> white.

%-----------------------------------------------------------
% Internal predicates
%
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

%   
% Accessors
%
language(Language):- language(Language, _, _, _, _).

month_name(Language, MonthNumber, MonthName):-
	language(Language, Months, _, _, _),
	nth1(MonthNumber, Months, MonthName).
  
week_day(Language, WeekNumber, WeekName):-
	language(Language, _, Weeks, _, _),
	nth1(WeekNumber, Weeks, WeekName).

adverb(Language, Today, Date, Date, today):-
	language(Language, _, _, Today, _).

adverb(Language, Tomorrow, Date, Next, tomorrow):-
	language(Language, _, _, _, Tomorrow), date_add(Date, days(1), Next).
