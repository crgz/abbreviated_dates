/*  Part of Epigrapher
    Author: Conrado Rodriguez
*/
:- module(configuration,[]).

:- use_module(library(settings)).

:- setting(epigrapher:port, integer, 3000, 'Default port').
:- setting(epigrapher:workers, integer, 10, 'Number of worker threads').
:- setting(epigrapher:debug, atom, 'http(request)', 'Enable debugging Topic').
:- setting(epigrapher:pidfile, atom, '/tmp/epigrapher.pid', 'Write the PID of the daemon process to this File').
:- setting(epigrapher:sighup, atom, 'quit', 'Action to perform on kill -HUP <pid>. ').
:- setting(epigrapher:fork, boolean, false, 'If given as --no-fork or --fork=false, the process runs in the foreground').
