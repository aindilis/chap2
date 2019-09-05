:- module(analysis,[a/0,analyze_position/2]).

:- dynamic holds/2, analyzed/1.

analyze_position(Pos) :-
	findall(pin(on(piece(Color1,Type1),square(X1,Y1)),on(piece(Color2,Type2),square(X2,Y2)),on(piece(Color2,Type3),square(X3,Y3)),Pos),
		concepts:pin(on(piece(Color1,Type1),square(X1,Y1)),on(piece(Color2,Type2),square(X2,Y2)),on(piece(Color2,Type3),square(X3,Y3)),Pos),
		Holds),

	%% board:attack(_A,on(_B,square(X,Y)),Pos),
	%% board:theoretical_range(piece(white,bishop),Purpose,square(c,1),square(X,Y),Pos),
	%% board:theoretical_range(piece(white,bishop),capture,square(c,1),square(X,Y),Pos),
	%% board:theoretical_range(piece(white,pawn),move,square(f,2),square(X,Y),Pos),
	%% board:controls_square(on(piece(white,bishop),square(c,1)),square(X,Y),Pos),
	%% board:legal_move(square(c,3),square(X,Y),Pos),

	forall(member(Fact,Holds),assert(holds(Fact,Pos))),
	assert(analyzed(Pos)).

a :-
	nl,write('Analysis:'),nl,nl.
a :-
	chess:current_pos(Pos),
	findall(Fact,holds(Fact,Pos),Facts),
	member(Fact,Facts),
	tab(4),write(Fact),nl,
	fail.
a :-
	nl,nl.


