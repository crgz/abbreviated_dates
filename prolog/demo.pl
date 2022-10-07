:- pack_install(abbreviated_dates).
:- use_module(library(abbreviated_dates)).
:- pack_install(cli_table).
:- use_module(library(cli_table)).

print_dates(Date):-
  findall([F,L,C],(abbreviated_dates:parse(date(2022,09,9),Date,[D],_,L,C),
  format_time(string(F),"%A, %d %b %Y",D)),Y),
  cli_table(Y,[head(['Date','Language','Country'])]).

print_dates('11-09, št').
