:- dynamic prolog_files/2, prolog_files/3, md/2, possible/1, execution/7.

:- multifile genlsDirectlyList/2.

:- ensure_loaded('/var/lib/myfrdcsa/codebases/minor/free-life-planner/lib/util/util.pl').

:- prolog_use_module(library(julian)).
:- prolog_use_module(library(regex)).
:- prolog_use_module(library(sha)).
:- prolog_use_module(library(make)).

:- ensure_loaded('/var/lib/myfrdcsa/codebases/minor/dates/frdcsa/sys/flp/autoload/dates.pl').
:- ensure_loaded('/var/lib/myfrdcsa/codebases/minor/free-life-planner/frdcsa/sys/flp/autoload/profile.pl').
:- ensure_loaded('/var/lib/myfrdcsa/codebases/minor/cyclone/frdcsa/sys/flp/autoload/kbs.pl').
:- ensure_loaded('/var/lib/myfrdcsa/codebases/minor/free-life-planner/lib/util/counter2.pl').
:- ensure_loaded('/var/lib/myfrdcsa/codebases/minor/free-life-planner/projects/microtheories/microtheory.pl').


flpFlag(not(debug)).

viewIf(Item) :-
 	(   flpFlag(debug) -> 
	    view(Item) ;
	    true).

testChap2 :-
	true.
	
:- use_module(chess).

:- log_message('DONE LOADING CHAP2.').

formalogModuleLoaded(chap2).

:- module(user).