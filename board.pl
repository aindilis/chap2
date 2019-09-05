:- module(board,[]).

:- use_module(concepts).

% I added tests, fixed a number of concepts related to move legality,
% added turns, fixed the change of state during piece movements, etc

% Still have a long ways to go until all move legality issues are
% worked out though.

% Still have to add legality checks and play turn transfer and also
% add special cases for castling, queening or similar, and en passant

:- multifile square/2, theoretical_range/5, on/2.

% :- op(50,xfy,in).
:- op(35,xfy,on).

% :- op(20,xf,white).
% :- op(20,xf,black).

% :- op(20,xf,a).
% :- op(20,xf,b).
% :- op(20,xf,c).
% :- op(20,xf,d).
% :- op(20,xf,e).
% :- op(20,xf,f).
% :- op(20,xf,g).
% :- op(20,xf,h).

% a Y :- square(a,Y).
% b Y :- square(b,Y).
% c Y :- square(c,Y).
% d Y :- square(d,Y).
% e Y :- square(e,Y).
% f Y :- square(f,Y).
% g Y :- square(g,Y).
% h Y :- square(h,Y).

% white Type :- piece(white,Type).
% black Type :- piece(black,Type).

square(a,1).
square(a,2).
square(a,3).
square(a,4).
square(a,5).
square(a,6).
square(a,7).
square(a,8).
square(b,1).
square(b,2).
square(b,3).
square(b,4).
square(b,5).
square(b,6).
square(b,7).
square(b,8).
square(c,1).
square(c,2).
square(c,3).
square(c,4).
square(c,5).
square(c,6).
square(c,7).
square(c,8).
square(d,1).
square(d,2).
square(d,3).
square(d,4).
square(d,5).
square(d,6).
square(d,7).
square(d,8).
square(e,1).
square(e,2).
square(e,3).
square(e,4).
square(e,5).
square(e,6).
square(e,7).
square(e,8).
square(f,1).
square(f,2).
square(f,3).
square(f,4).
square(f,5).
square(f,6).
square(f,7).
square(f,8).
square(g,1).
square(g,2).
square(g,3).
square(g,4).
square(g,5).
square(g,6).
square(g,7).
square(g,8).
square(h,1).
square(h,2).
square(h,3).
square(h,4).
square(h,5).
square(h,6).
square(h,7).
square(h,8).

rank(1).
rank(2).
rank(3).
rank(4).
rank(5).
rank(6).
rank(7).
rank(8).

file(a).
file(b).
file(c).
file(d).
file(e).
file(f).
file(g).
file(h).

diagonal(negative,7).
diagonal(negative,6).
diagonal(negative,5).
diagonal(negative,4).
diagonal(negative,3).
diagonal(negative,2).
diagonal(negative,1).
diagonal(negative,0).
diagonal(negative,-1).
diagonal(negative,-2).
diagonal(negative,-3).
diagonal(negative,-4).
diagonal(negative,-5).
diagonal(negative,-6).
diagonal(negative,-7).

diagonal(positive,16).
diagonal(positive,15).
diagonal(positive,14).
diagonal(positive,13).
diagonal(positive,12).
diagonal(positive,11).
diagonal(positive,10).
diagonal(positive,9).
diagonal(positive,8).
diagonal(positive,7).
diagonal(positive,6).
diagonal(positive,5).
diagonal(positive,4).
diagonal(positive,3).
diagonal(positive,2).

corner_square(square(a,1)).
corner_square(square(h,1)).
corner_square(square(a,8)).
corner_square(square(h,8)).

center_square(square(d,4)).
center_square(square(d,5)).
center_square(square(e,4)).
center_square(square(e,5)).

abbreviation(pawn,'P').
abbreviation(rook,'R').
abbreviation(knight,'N').
abbreviation(bishop,'B').
abbreviation(queen,'Q').
abbreviation(king,'K').

hasValue(pawn,1).
hasValue(knight,2.5).
hasValue(bishop,3.5).
hasValue(rook,5).
hasValue(queen,9).
hasValue(king,99).

file_to_int(a,1).
file_to_int(b,2).
file_to_int(c,3).
file_to_int(d,4).
file_to_int(e,5).
file_to_int(f,6).
file_to_int(g,7).
file_to_int(h,8).

square(X,Y) :-
	file(X),
	rank(Y).

% means X3,Y3 is in between X1,Y1 and X2,Y2
lies_between(square(X1,Y1),square(X3,Y3),square(X2,Y2)) :-
	colinear(square(X1,Y1),square(X2,Y2),Element),
	colinear(square(X1,Y1),square(X3,Y3),Element),
	square_distance(square(X1,Y1),square(X2,Y2),Z12),
	square_distance(square(X1,Y1),square(X3,Y3),Z13),
	square_distance(square(X2,Y2),square(X3,Y3),Z23),
	Z12 > max(Z13,Z23).

colinear(square(X1,Y1),square(X2,Y2),File) :-
	file(X2),
	rank(Y2),
	file(File),
	on_file(square(X1,Y1),File),
	on_file(square(X2,Y2),File).

colinear(square(X1,Y1),square(X2,Y2),Rank) :-
	file(X2),
	rank(Y2),
	rank(Rank),
	on_rank(square(X1,Y1),Rank),
	on_rank(square(X2,Y2),Rank).

colinear(square(X1,Y1),square(X2,Y2),Diagonal) :-
	file(X2),
	rank(Y2),
	diagonal(_Sign,_Int) = Diagonal,
	on_diagonal(square(X1,Y1),Diagonal),
	on_diagonal(square(X2,Y2),Diagonal).

on_file(square(X,_Y),File) :-
	X = File.

on_rank(square(_X,Y),Rank) :-
	Y = Rank.

on_diagonal(square(X,Y),Diagonal) :-
	file_to_int(X,A),
	Z is A + Y,
	Diagonal = diagonal(positive,Z).

on_diagonal(square(X,Y),Diagonal) :-
	file_to_int(X,A),
	Z is A - Y,
	Diagonal = diagonal(negative,Z).

square_distance(square(X1,Y1),square(X2,Y2),Z) :-
	abs_distance(X1,X2,Z1),
	abs_distance(Y1,Y2,Z2),
	Z is max(Z1,Z2).

distance(X,Y,Z) :-
	file(X),
	file(Y),
	file_to_int(X,A),
	file_to_int(Y,B),
	Z is B - A.

distance(X,Y,Z) :-
	rank(X),
	rank(Y),
	Z is Y - X.

distance(Color,X,Y,Z) :-
	Color = white,
	rank(X),
	rank(Y),
	Z is Y - X.

distance(Color,X,Y,Z) :-
	Color = black,
	rank(X),
	rank(Y),
	distance(white,X,Y,A),
	Z is -A.

abs_distance(X,Y,Z) :-
	distance(X,Y,A),
	Z is abs(A).

theoretical_range(piece(_Color,knight),_Purpose,square(X1,Y1),square(X2,Y2),_Pos) :-
	abs_distance(X1,X2,1),
	abs_distance(Y1,Y2,2).

theoretical_range(piece(_Color,knight),_Purpose,square(X1,Y1),square(X2,Y2),_Pos) :-
	abs_distance(X1,X2,2),
	abs_distance(Y1,Y2,1).

theoretical_range(piece(_Color,king),_Purpose,square(X1,Y1),square(X2,Y2),_Pos) :-
	not(same_square(square(X1,Y1),square(X2,Y2))),
 	abs_distance(X1,X2,Z1),
 	abs_distance(Y1,Y2,Z2),
 	Z1 < 2,
 	Z2 < 2.

same_square(square(X1,Y1),square(X2,Y2)) :-
	X1 = X2,
	Y1 = Y2.

different_square(square(X1,Y1),square(X2,Y2)) :-
	X1 \= X2 ; Y1 \= Y2.

occupied(square(X1,Y1),Pos) :-
	position:in(on(piece(_Color,_Type),square(X1,Y1)),Pos).

theoretical_range(piece(white,pawn),move,square(X,2),square(X,4),Pos) :-
	not(occupied(square(X,3),Pos)),
	not(occupied(square(X,4),Pos)).

theoretical_range(piece(black,pawn),move,square(X,7),square(X,5),Pos) :-
	not(occupied(square(X,6),Pos)),
	not(occupied(square(X,5),Pos)).

theoretical_range(piece(Color,pawn),move,square(X,Y1),square(X,Y2),Pos) :-
	not(occupied(square(X,Y2),Pos)),
	distance(Color,Y1,Y2,1).

theoretical_range(piece(Color,pawn),capture,square(X1,Y1),square(X2,Y2),_Pos) :-
	abs_distance(X1,X2,1),
	distance(Color,Y1,Y2,1).

theoretical_range(piece(_Color,rook),_Purpose,square(X,Y1),square(X,Y2),_Pos) :-
	not(Y1 = Y2).

theoretical_range(piece(_Color,rook),_Purpose,square(X1,Y),square(X2,Y),_Pos) :-
	not(X1 = X2).

theoretical_range(piece(_Color,bishop),_Purpose,square(X1,Y1),square(X2,Y2),_Pos) :-
	not(same_square(square(X1,Y1),square(X2,Y2))),
	abs_distance(Y1,Y2,Z1),
	abs_distance(X1,X2,Z2),
	Z1 = Z2.

theoretical_range(piece(Color,queen),Purpose,square(X1,Y1),square(X2,Y2),Pos) :-
	theoretical_range(piece(Color,bishop),Purpose,square(X1,Y1),square(X2,Y2),Pos).

theoretical_range(piece(Color,queen),Purpose,square(X1,Y1),square(X2,Y2),Pos) :-
	theoretical_range(piece(Color,rook),Purpose,square(X1,Y1),square(X2,Y2),Pos).

% we will use the following to represent the diagonals

line_blocked(piece(_Color,Type),square(X1,Y1),square(X2,Y2),Pos) :-
	not(Type = knight),
	lies_between(square(X1,Y1),square(X3,Y3),square(X2,Y2)),
	position:in(on(_Piece,square(X3,Y3)),Pos).

controls_square(on(piece(Color1,Type1),square(X1,Y1)), square(X2,Y2), Pos) :-
	position:in(on(piece(Color1,Type1),square(X1,Y1)),Pos),
	theoretical_range(piece(Color1,Type1),capture,square(X1,Y1),square(X2,Y2),pos),
	not(line_blocked(piece(Color1,Type1),square(X1,Y1),square(X2,Y2),Pos)).

thereExistAtLeastOne(X) :-
	\+ \+ X.

controlled_square(square(X2,Y2),Pos) :-
	square(X2,Y2),
	thereExistAtLeastOne(controls_square(piece(_Color1,_Type1) on square(_X1,_Y1), square(X2,Y2), Pos)).

attack(on(piece(Color1,Type1),square(X1,Y1)),on(piece(Color2,Type2),square(X2,Y2)), Pos) :-
	position:in(on(piece(Color2,Type2),square(X2,Y2)),Pos),
	controls_square(on(piece(Color1,Type1),square(X1,Y1)), square(X2,Y2),Pos),
	opposite_color(Color1,Color2).

in_check(Color2,Pos) :-
	attack(on(piece(Color1,_Type1),square(_X1,_Y1)),on(piece(Color2,king),square(_X2,_Y2)),Pos),
	Color2 \= Color1.

opposite_color(Color1,Color2) :-
	(   Color1 = white -> Color2 = black ;
	    (	Color1 = black -> Color2 = white ; fail)).

% NON-PAWN PIECE MOVEMENT & CAPTURE
legal_move(square(X1,Y1),square(X2,Y2),Pos) :-
	controls_square(on(piece(Color,Type1),square(X1,Y1)),square(X2,Y2),Pos),
	Type1 \= pawn,
	not(position:in(on(piece(Color,_Type2),square(X2,Y2)),Pos)).

% PAWN CAPTURE
legal_move(square(X1,Y1),square(X2,Y2),Pos) :-
  	controls_square(on(piece(Color1,pawn),square(X1,Y1)),square(X2,Y2),Pos),
 	opposite_color(Color1,Color2),
  	position:in(on(piece(Color2,_Type2),square(X2,Y2)),Pos).

% PAWN MOVEMENT
legal_move(square(X1,Y1),square(X2,Y2),Pos) :-
	position:in(on(piece(Color,pawn),square(X1,Y1)),Pos),
	theoretical_range(piece(Color,pawn),move,square(X1,Y1),square(X2,Y2),Pos),
	not(line_blocked(piece(Color,pawn),square(X1,Y1),square(X2,Y2),Pos)),
	not(position:in(on(piece(Color,_Type),square(X2,Y2)),Pos)).

% hasPreviouslyMoved(piece(Color,Type) on square(X,Y) in pos1) :-
% 	false.

% permissible(castle(piece(Color,king) on square(e,Y) in Pos, piece(Color,rook) on square(X,Y) in Pos)) :-
% 	X = a,

% I just thought of a sort of interesting approach to an unrelated
% problem.  I considered that the way the solvers compute the AI is by
% having enough constraints to delineate possible AIs.  For instance,
% in this situation, this is a correct move.  Or, larger programs are
% heuristically better than smaller programs.  etc.  So it's
% essentially a rule-based approach.  But recently, rule-based
% approaches haven fallen out of favor due to neural approaches, like
% AlphaZero.  Well here is what might be done to bootstrap rules from
% AlphaZero.  Look at the play it is making in terms of how rule-based
% systems interpret it.  So like, do a rule-based positional analysis
% of the position the AI takes in a chess game, using predicates
% translated into the board representation from the professional Chess
% analysis texts, and then learn rules which relate the analysis
% predicates to the positions taken up.

:- M = board, ignore((source_file(M:P,_), functor(P,F,A), M:export(M:F/A), fail)).
