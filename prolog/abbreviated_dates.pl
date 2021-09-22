%-----------------------------------------------------------
% Module definition
%

:- module(abbreviated_dates, [parse/4]).   % get a date

%-----------------------------------------------------------
% Native dependency requirements
%

:- [library(dcg/basics)].
:- use_module(library(date_time)).
:- use_module(library(dcg/basics), [whites//0, nonblanks//1, digits//1]).


:- [facts/languages]. % Facts about languages

% 
% date_time:date_get(today, Today), parse(Today, 'Freitag, 7. Mai', Dates, T).
% 
% 
parse(Context, Expression, Dates, Trace) :-
  atom_codes(Expression, Codes), phrase(multipleDays([Context], Dates, Trace), Codes).

multipleDays(_, [], []) --> [].
multipleDays([LastKnownDate|Other], [SingleDay|MultipleDays], [T1|T2]) -->
  singleDay([LastKnownDate|Other], SingleDay, T1),
  (dash | eos),
  multipleDays([SingleDay, LastKnownDate|Other], MultipleDays, T2).

singleDay([Context|_], date(Y, M, D), dm(Language)) -->
  dayNumber(D), b, month(M, Language), {bestYear(Context, M, D, Y)}.

singleDay([Context|_], Date, d(unknown)) -->
  dayNumber(D), {bestDate(Context, D, Date)}.

singleDay([Context|_], date(Y, M, D), md(Language)) -->
  month(M, Language), b, dayNumber(D), {bestYear(Context, M, D, Y)}.

singleDay([Context|_], Date, adverb(Language)) -->
  nonblanks(Codes),
  {atom_codes(Adverb, Codes), adverb(Language, Adverb, Context, Date)}.

singleDay(Context, Date, sd(Language, T)) -->
  weekDay(_, Language), b, singleDay(Context, Date, T).

weekDay(D, wd(Language)) -->
  string(Codes), ",",
  {atom_codes(WeekDay, Codes), dayName(Language, D, WeekDay)}.

dayNumber(D) --> integer(D).
dayNumber(D) --> integer(D), ".".

month(MonthNumber, explicit(Language)) -->
  nonblanks(Codes),
  { atom_codes(MonthName, Codes), month(Language, MonthNumber , MonthName) }.

month(MonthNumber, abbreviated(Language)) -->
  string(Abbreviation), ".",
  { month(Language, MonthNumber, MonthName),
    atom_codes(MonthName, Codes),
    append(Abbreviation, Tail, Codes),
    length(Tail,L),
    L > 0
  }.

bestDate(date(Year, 12, D), Day, date(BestYear, BestMonth, Day)):-
  ( Day >= D -> BestYear = Year, BestMonth = 12; BestYear is Year + 1, BestMonth = 1).

bestDate(date(Year, Month, D), Day, date(Year, BestMonth, Day)):-
  ( Day >= D -> BestMonth = Month; BestMonth is Month + 1).

bestYear(Context, Month, Day, Year) :-
  date_extract(Context, years(Y)),
  ( date_compare(date(Y, Month, Day), >=, Context) -> Year = Y; Year is Y + 1).

dash --> " - ".
b --> white.


month(Language, MonthNumber, MonthName):-
	language(Language, Months, _, _, _),
	nth1(MonthNumber, Months, MonthName).
  
dayName(Language, WeekNumber, WeekName):-
	language(Language, _, Weeks, _, _),
	nth1(WeekNumber, Weeks, WeekName).

adverb(Language, Today, Date, Date):-
	language(Language, _, _, Today, _).

adverb(Language, Tomorrow, Date, Next):-
	language(Language, _, _, _, Tomorrow), date_add(Date, days(1), Next).

