:- module(position,[in/2]).

:- op(50,xfy,in).
:- op(35,xfy,on).

turn(white) in pos(1).

piece(white,rook) on square(a,1) in pos(1).
piece(white,knight) on square(b,1) in pos(1).
piece(white,bishop) on square(c,1) in pos(1).
piece(white,queen) on square(d,1) in pos(1).
piece(white,king) on square(e,1) in pos(1).
piece(white,bishop) on square(f,1) in pos(1).
piece(white,knight) on square(g,1) in pos(1).
piece(white,rook) on square(h,1) in pos(1).
piece(white,pawn) on square(a,2) in pos(1).
piece(white,pawn) on square(b,2) in pos(1).
piece(white,pawn) on square(c,2) in pos(1).
piece(white,pawn) on square(d,2) in pos(1).
piece(white,pawn) on square(e,2) in pos(1).
piece(white,pawn) on square(f,2) in pos(1).
piece(white,pawn) on square(g,2) in pos(1).
piece(white,pawn) on square(h,2) in pos(1).
piece(black,rook) on square(a,8) in pos(1).
piece(black,knight) on square(b,8) in pos(1).
piece(black,bishop) on square(c,8) in pos(1).
piece(black,queen) on square(d,8) in pos(1).
piece(black,king) on square(e,8) in pos(1).
piece(black,bishop) on square(f,8) in pos(1).
piece(black,knight) on square(g,8) in pos(1).
piece(black,rook) on square(h,8) in pos(1).
piece(black,pawn) on square(a,7) in pos(1).
piece(black,pawn) on square(b,7) in pos(1).
piece(black,pawn) on square(c,7) in pos(1).
piece(black,pawn) on square(d,7) in pos(1).
piece(black,pawn) on square(e,7) in pos(1).
piece(black,pawn) on square(f,7) in pos(1).
piece(black,pawn) on square(g,7) in pos(1).
piece(black,pawn) on square(h,7) in pos(1).

%% :- M = board, ignore((source_file(M:P,_), functor(P,F,A), M:export(M:F/A), fail)).
