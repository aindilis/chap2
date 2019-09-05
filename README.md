CHess Analysis Program version 2

CHAP is a program for positional theorem proving in chess.  It uses a
provably superior, but computationally less tractable, approach to
playing chess.  The idea is to learn cross-domain relationships from
chess to real life.  CHAP2 uses the Formalog system to enable
integration between Prolog and Perl, to make it that much easier to
develop for it.  The advantage of CHAP2 is a better approach to using
Prolog (using Language::Prolog::Yaswi) than was possible with the
original CHAP program (using AI::Prolog).

There are two ways to run it, one using Formalog, and one without.  To
use Formalog, you need the rest of the FRDCSA, which is available from
https://github.com/aindilis, but requires about 4 hours of
installation.  A VM is being prepared.

In lieu of using Formalog, there is the standalone version, which
works just as well for most purposes.

simply do

./standalone.sh

Currently, to load a position, you can do:

?- game:test_game.

In order to run the analysis mode, which currently is very limited, run:

? analysis:analyze_position(pos(7)).

Then, to display the results, run:

?- d.

You will then hopefully see this:

```

?- d.

  +----+----+----+----+----+----+----+----+
8 | -R-| -N-|    | -Q-| -K-| -B-| -N-| -R-|
  +----+----+----+----+----+----+----+----+
7 | -P-| -P-| -P-|    |    | -P-| -P-| -P-|
  +----+----+----+----+----+----+----+----+
6 |    |    |    | -P-|    |    |    |    |
  +----+----+----+----+----+----+----+----+
5 |    |    |    |    | -P-|    |    |    |
  +----+----+----+----+----+----+----+----+
4 |    |    |    |    | |P||    |1-B-|    |
  +----+----+----+----+----+----+----+----+
3 |    |    | |P||    |    |2|N||    |    |
  +----+----+----+----+----+----+----+----+
2 | |P|| |P||    | |P||    | |P|| |P|| |P||
  +----+----+----+----+----+----+----+----+
1 | |R|| |N|| |B||3|Q|| |K|| |B||    | |R||
  +----+----+----+----+----+----+----+----+
    a    b    c    d    e    f    g    h

[turn,white]


Analysis:

    pin(on(piece(black,bishop),square(g,4)),on(piece(white,knight),square(f,3)),on(piece(white,queen),square(d,1)),pos(7)) 


true.

?-

```

You will note that it has highlighted the pin.

To make moves, simply use this notation:

?- m(d2d4).

