:- module(chess,[a/0,d/0,m/1,move/4,copy_pos/2]).

:- use_module(board).
:- use_module(position).
:- use_module(display).
:- use_module(game).
:- use_module(analysis).

:- dynamic current_pos/1.

:- multifile in/2.

move(Square1,_,Pos1,_) :-
	position:in(on(X,Square1),Pos1),
	write(X),
	fail.

move(Square1,Square2,Pos1,Pos2) :-
	copy_pos(Pos1,Pos2),
	position:in(on(X,Square1),Pos2),
	(   position:in(on(Y,Square2),Pos2) -> retract(position:in(on(Y,Square2),Pos2)) ; true),
	retract(position:in(on(X,Square1),Pos2)),
	assert(position:in(on(X,Square2),Pos2)),
	retract(position:in(turn(Color1),Pos2)),
	board:opposite_color(Color1,Color2),
	assert(position:in(turn(Color2),Pos2)),
	display:print_board(Pos2),
	!.

move(Square1,_Square2,_Pos1,_Pos2) :-
	write('no piece on '),
	write(Square1),
	nl.

copy_pos(Pos1,Pos2) :-
	forall(position:in(PieceOnSquare,Pos1), assert(position:in(PieceOnSquare,Pos2))).

wipe(Pos) :-
	retractall(position:in(_X,Pos)).

get_next_position(pos(N),pos(M)) :-
	M is N + 1.

current_pos(pos(1)).

mv(Square1,Square2) :-
	retract(current_pos(Pos1)),
	get_next_position(Pos1,Pos2),
	move(Square1,Square2,Pos1,Pos2),
	assert(current_pos(Pos2)).

m(String) :-
	atom_chars(String,[X1,Y1,X2,Y2]),
	atom_number(Y1,NY1),
	atom_number(Y2,NY2),
	mv(square(X1,NY1),square(X2,NY2)).

d :-
	current_pos(Pos),
	(   not(analysis:analyzed(Pos)) -> analysis:analyze_position(Pos) ; true),
	display:print_board(Pos),!,a.

%% get_pos_suffix(Pos,N) :-
%% 	atom_length(Pos,LP),
%% 	L is LP - 3,
%% 	sub_atom(Pos, 3, L, A, N).

%% get_next_position(Pos1,Pos2) :-
%% 	get_pos_suffix(Pos1,AN),
%% 	atom_number(AN,N),
%% 	M is N + 1,
%% 	atom_number(AM,M),
%% 	atomic_list_concat([pos,AM],'',Pos2).

%% :- equate(pos1,pos2).

%% :- move(square(d,2),square(d,4),pos1,pos2).

:- display:print_board(pos(1)).
