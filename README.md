# game-solver-template

# Project Grade:         59/100
## Functionality               40/73
* Game mechanics:              16/20
  * isValidMove is broken, and thus breaks whoWillWin
  * showBoard is sideways
  * checkDiagonal only checks diagonals in one direction. Reversing the board fixes.
* Exact game solver:           13/15
  * whoWillWin works once isValidMove and checkWin are fixed, although it doesn't prefer ties over
    losses.
  * bestMove does work, although it recomputes the outcomes outside of the tuple for some reason.
* Cut-off depth solver:        3/13
  * ratings exist, and there is some stuff that touches on minimax, but almost all unimplemented
* Evaluation function:         0/2
* Avoiding unnecessary work:   0/3
* Command-line interface:      6/10
  * readgame doesn't seem to work
  * Test inputs don't work
  * didn't use getOpt, so overly sensitive
* Move and verbose flags:      0/5
* Error-handling:              2/5
  * None on readGame, bestMove

## Design                      19/27
* Well-designed data types:    8
  * WholeBoard and Board duplicate eachother
* Well-decomposed functions:   10
  * Helper functions shadowing topLevel functions (evaluateMove).
  * Often unabstracted functions (declarePotential. which is also a confusing name)
  * 
* Good module decomposition:   2
  * only two modules for the bulk of the code is very light. Test modules are nice, but don't seem
    to work. 
* Good variable names:         2
* Efficient/idiomatic code:    5
