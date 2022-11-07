:- module(requirements, [install/0, install/1, remove/0]).

default([tap,date_time,abbreviated_dates]).

install :- default(R), install(R).

install(R) :-
    open_null_stream(Null),
    set_prolog_IO(user_input, user_output, Null),
    foreach((member(P,R),missing(P)),pack_install(P,[interactive(false)])),
    close(Null). 

missing(P):-
    getenv('HOME',H),
    atomic_list_concat([H, '/.local/share/swi-prolog/pack/', P], D),
    \+exists_directory(D).

remove :-
    default(R),
    reverse(R, Reverse),
    foreach((member(P,Reverse), exist(P)), pack_remove(P)).

exist(P):- pack_property(P, library(P)).
