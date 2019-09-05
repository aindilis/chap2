%% The player whose King is checked then has to make a move which gets the King out of check +---------------------------------------+ 8 | | | | | | | #K | | |---------------------------------------| 7 | | | | | | #P | #P | | |---------------------------------------| 6 | | | | #B | | #Kt| | #P | |---------------------------------------| 5 | | | | | | | | | |---------------------------------------| 4 | ^P | | | | | | | | |---------------------------------------| 3 | | | ^P | | | | | ^P | |---------------------------------------| 2 | | | | | | | ^P | | |---------------------------------------| 1 | | | | | ^R | | ^K | | +---------------------------------------+ a b c d e f g h DIAGRAM 5. or he forfeits the game.
sentence('chessAndCheckers_TheBook',sentenceIdFn(219),'The player whose King is checked then has to make a move which gets the King out of check +---------------------------------------+ 8 | | | | | | | #K | | |---------------------------------------| 7 | | | | | | #P | #P | | |---------------------------------------| 6 | | | | #B | | #Kt| | #P | |---------------------------------------| 5 | | | | | | | | | |---------------------------------------| 4 | ^P | | | | | | | | |---------------------------------------| 3 | | | ^P | | | | | ^P | |---------------------------------------| 2 | | | | | | | ^P | | |---------------------------------------| 1 | | | | | ^R | | ^K | | +---------------------------------------+ a b c d e f g h DIAGRAM 5. or he forfeits the game.').
hasFormalization('chessAndCheckers_TheBook',sentenceFn(219),
                 [
		  implies(inCheck(Player1,Pos1),must(and(next(Pos1,Pos2),not(inCheck(Player2,Pos2)))))
                 ]).

%% This is the only case in which a player is not at liberty to make any move he likes.
sentence('chessAndCheckers_TheBook',sentenceIdFn(220),'This is the only case in which a player is not at liberty to make any move he likes.').
hasFormalization('chessAndCheckers_TheBook',sentenceFn(220),
                 [
                  _
                 ]).

%% Unless the attacking man can be captured there are only two ways of getting out of check.
sentence('chessAndCheckers_TheBook',sentenceIdFn(221),'Unless the attacking man can be captured there are only two ways of getting out of check.').
hasFormalization('chessAndCheckers_TheBook',sentenceFn(221),
                 [
		  formalizePredicates([attackingPiece])
                  hasMethod(gettingOutOfCheck,capture(attackingPiece))
                 ]).

%% One of these is to interpose a man between the King and the attacking piece, and the other to move the King out of the line of attack.
sentence('chessAndCheckers_TheBook',sentenceIdFn(222),'One of these is to interpose a man between the King and the attacking piece, and the other to move the King out of the line of attack.').
hasFormalization('chessAndCheckers_TheBook',sentenceFn(222),
                 [
                  hasMethod(gettingOutOfCheck,interposeBetween(Piece,friendlyFn(King),attackingPiece)),
		  hasMethod(gettingOutOfCheck,move(friendlyFn(King),Square),not(in(Square,lineOfAttack)))
                 ]).

%% In Diagram 5 Black could give check by moving the Bishop to c5.
sentence('chessAndCheckers_TheBook',sentenceIdFn(223),'In Diagram 5 Black could give check by moving the Bishop to c5.').
hasFormalization('chessAndCheckers_TheBook',sentenceFn(223),
                 [
		  equals(situation1,in(diagramPositionFn(digram(5),Pos),by(can(Black,giveCheck),move(Bishop,square(c,5)))))
                  _
                 ]).

%% In answer to this White has four moves at his disposal.
sentence('chessAndCheckers_TheBook',sentenceIdFn(224),'In answer to this White has four moves at his disposal.').
hasFormalization('chessAndCheckers_TheBook',sentenceFn(224),
                 [
		  hasCardinality(Move,inAnswerTo(situation1,Move),4)
                  _
                 ]).

%% He may either move the King to f1 or h1 or h2, or he may interpose his Rook on e3.
sentence('chessAndCheckers_TheBook',sentenceIdFn(225),'He may either move the King to f1 or h1 or h2, or he may interpose his Rook on e3.').
hasFormalization('chessAndCheckers_TheBook',sentenceFn(225),
                 [
                  equals(situation2,can(White,or(move(pieceFn(king),or(f1,h1,h2)),interpose(pieceFn(rook),e3))))
                 ]).

%% The latter would be very unwise as Black would simply take the Rook with his Bishop, again checking White's King.
sentence('chessAndCheckers_TheBook',sentenceIdFn(226),'The latter would be very unwise as Black would simply take the Rook with his Bishop, again checking White''s King.').
hasFormalization('chessAndCheckers_TheBook',sentenceFn(226),
                 [
                  inSituation(situation2,isVeryUnwhiseBecause(do(White,interpose(pieceFn(rook),e3)),and(takeWith(black,the(rook),his(bishop)),isa(nextPosFn(situation2),inCheck))))
                 ]).

%% The situation would then not have changed at all except that White would have lost his Rook.
sentence('chessAndCheckers_TheBook',sentenceIdFn(227),'The situation would then not have changed at all except that White would have lost his Rook.').
hasFormalization('chessAndCheckers_TheBook',sentenceFn(227),
                 [
		  except(not(change(situation)),lost(white,his(rook)))
                 ]).

%% White's King could not move to f2, for this would leave him still attacked by the Bishop.
sentence('chessAndCheckers_TheBook',sentenceIdFn(228),'White''s King could not move to f2, for this would leave him still attacked by the Bishop.').
hasFormalization('chessAndCheckers_TheBook',sentenceFn(228),
                 [
                  because(not(could(white(king),moveTo(f2))),would(this,still(attackedBy(white(king),the(black(bishop))))))
                 ]).

%% Instead of checking on c5 Black could have attacked White's King on h2.
sentence('chessAndCheckers_TheBook',sentenceIdFn(229),'Instead of checking on c5 Black could have attacked White''s King on h2.').
hasFormalization('chessAndCheckers_TheBook',sentenceFn(229),
                 [
		  insteadOf(checkingOn(c5),could(black,attack(on(white(king),h2))))
                 ]).
