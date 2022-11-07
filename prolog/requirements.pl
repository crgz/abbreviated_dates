:- module(requirements, [install/0, install/1, remove/0]).

default([tap,date_time,abbreviated_dates]).

install :- default(Requirements), install(Requirements).

install(Requirements) :-
    open_null_stream(Null),
    set_prolog_IO(user_input, user_output, Null),
    foreach((member(Pack,Requirements),missing(Pack)),pack_install(Pack,[interactive(false)])),
    close(Null). 

missing(Pack):-
    getenv('HOME',Home),
    atomic_list_concat([Home, '/.local/share/swi-prolog/pack/', Pack], Directory),
    \+exists_directory(Directory).

remove :-
    default(Requirements),
    reverse(Requirements, Reverse),
    foreach((member(Pack,Reverse), exist(Pack)), pack_remove(Pack)).

exist(Pack):- pack_property(Pack, library(Pack)).
