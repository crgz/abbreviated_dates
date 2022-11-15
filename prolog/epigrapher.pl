:- module(epigrapher, [server/0, parse_parameters/3, parse_time_fail_safe/2]).

:- use_module(library(http/http_unix_daemon)).
:- use_module(library(http/http_error)). % https://www.swi-prolog.org/pldoc/man?section=http-debug
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_log)).
:- use_module(library(date_time)).
:- use_module(library(abbreviated_dates)).
:- use_module(configuration).

:- set_setting(http:logfile, '/dev/stdout').
:- http_handler(root(api/analyze), analyze, []).
:- http_handler(root(health), server_health, []).

server :-
	server([]).

server(Options) :-
    findall(Term, (setting(epigrapher:Functor, Value),Term=..[Functor,Value]), Defaults),
	merge_options(Options, Defaults, HTTPOptions),
	http_daemon(HTTPOptions).

%%	analyze(+Request)
%
%	HTTP handler that retrieve possible date graphemes meanings, analyzeing their uses according to cultural contexts
%   curl -XPOST -H "Content-Type: application/json" localhost:3000/api/analyze -d '{"from":"2021-01-26","grapheme":"11-09, št"}'
analyze(Request) :-
    http_read_json_dict(Request, Dict),
    analyze(Dict, Meanings),
    reply_json_dict( _{'meanings':Meanings}).

analyze(Dict, Meanings) :-
  parse_parameters(Dict, Starting, Grapheme),
  findall(Meaning, format(Starting,Grapheme,Meaning), Meanings).

parse_parameters(_{from:From,grapheme:Grapheme}, Starting, Grapheme):- parse_time_fail_safe(From, Starting).
parse_parameters(_{          grapheme:Grapheme}, Starting, Grapheme):- date_get(today, Starting).

parse_time_fail_safe(From, date(Year, Month, Day)):-
  parse_time(From, Stamp), !,
  stamp_date_time(Stamp, date(Year, Month, Day, _, _, _, _, _, _), 'UTC').
parse_time_fail_safe(_, Starting):-date_get(today, Starting).

format(Starting, Grapheme, Meaning):-
  parse(Starting, Grapheme, [Date], [Format], Language, Country),
  format_time(string(DateText), "%FT%T%z", Date),
  dict_pairs(Meaning, _, [date-DateText,language-Language,country-Country,format-Format]).

%%	server_health(+Request)
%
%	HTTP handler that replies with the overall health of the server
server_health(_) :- reply_json( _{'up':true}).
