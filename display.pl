:- module(display,[]).

:- use_module(board).
:- use_module(position).

print_board(Pos) :-
	findall(R,board:rank(R),Ranks),
	reverse(Ranks,RRanks),
	member(Y,RRanks),
	file_separator,
	write(Y),
	write(' |'),
	print_rank(Pos,Y),
	fail.

print_board(_Pos) :-
	file_separator,
	write('    a    b    c    d    e    f    g    h'),
	nl,nl,
	fail.

print_board(Pos) :-
	board:in_check(Color,Pos),
	write([in_check,Color]),
	nl,
	fail.

print_board(Pos) :-
	position:in(turn(Color),Pos),
	write([turn,Color]),
	nl,nl.

file_separator :-
	nl,
	% write('  +---+---+---+---+---+---+---+---+'),
	write('  +----+----+----+----+----+----+----+----+'),
	nl.

print_rank(Pos,Y) :-
	board:file(X),
	print_special(Pos,X,Y),
	print_square(Pos,X,Y),
	write('|'),
	fail.

print_square(Pos,X,Y) :-
	position:in(on(piece(Color,Type),square(X,Y)),Pos),
	Color = white,
	write('|'),
	abbreviation(Type,B),
	write(B),
	write('|'),
	!.

print_square(Pos,X,Y) :-
	position:in(on(piece(Color,Type),square(X,Y)),Pos),
	Color = black,	
	write('-'),
	abbreviation(Type,B),
	write(B),
	write('-'),
	!.

print_square(_,_,_) :-
	write('   '),
	!.

print_special(Pos,X,Y) :-
	analysis:holds(pin(on(_P1,square(X1,Y1)),on(_P2,square(X2,Y2)),on(_P3,square(X3,Y3)),Pos),Pos),
	(   (	X = X1, Y = Y1 ) -> write('1') ;
	    (	(   X = X2, Y = Y2 ) -> write('2') ;
		(   (	X = X3, Y = Y3 ) -> write('3') ; fail))),
	%% write('*'),
	!.

print_special(_Pos,_X,_Y) :-
	write(' ').

:- M = display, ignore((source_file(M:P,_), functor(P,F,A), M:export(M:F/A), fail)).