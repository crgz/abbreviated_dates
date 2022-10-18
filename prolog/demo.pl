:- use_module(library(abbreviated_dates)).
:- use_module(library(cli_table)).

solutions(Text):- % E.g. solutions('11-09, št').
  Starting = date(2022,09,9),
  findall([Date,Language,Country],format(Starting,Text,Date,Language,Country),Row),
  cli_table(Row,[head(['Date','Language','Country'])]).

format(Starting, Text, DateText, Language, Country):-
  parse(Starting, Text, [Date], _, Language, Country),
  format_time(string(DateText), "%A, %d %b %Y", Date).
