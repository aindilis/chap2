:- use_module(board).

:- use_module(position).

:- begin_tests(board).

test(distance) :-
	distance(black,1,3,-2).

test(square) :-
	square(a,1).

test(theoretical_range,[nondet]) :-
 	findall([X2,Y2],theoretical_range(piece(white,pawn),move,square(e,2),square(X2,Y2),pos1),Results),
	write([results,Results]),
	Results = [[e,4],[e,3]].

:- end_tests(board).
