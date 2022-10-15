:- use_module(library(abbreviated_dates)).
:- use_module(library(cli_table)).

solutions(Date):-
  findall([F,L,C],(abbreviated_dates:parse(date(2022,09,9),Date,[D],_,L,C),
  format_time(string(F),"%A, %d %b %Y",D)),Y),
  cli_table(Y,[head(['Date','Language','Country'])]).

solutions('11-09, št').
