:- module(game,[test_game/0]).

clean_slate :-
	true.

load_game(Contents,_Pos) :-
	clean_slate,
	member(Move,Contents),
	chess:m(Move),
	fail.
load_game(Contents,Pos) :-
	chess:current_pos(Pos).

test_game :-
	%% load_game([e2e4,e7e5,g1f3,b8c6,f1c4,f8c5,c2c3,g8f6,d2d4,e5d4,e4d5,f6e4,c4d5,e4f2,e1f2,d4c3,f2f1,c3b2,c1b2],Pos).
	load_game([e2e4, e7e5, g1f3, d7d6, c2c3, c8g4],Pos).
