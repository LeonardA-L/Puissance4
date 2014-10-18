% Yes
%:- [ai1].
:- [map].
:- use_module(ai1, [play/1 as aiplayA]).
:- use_module(ai2, [play/1 as aiplayB]).

:- nb_setval(turn, 0).

victory(X):- X>10, write('victory'),write(X).


playModulo(X) :- B is (X mod 2)+1, playTurn(B).

playTurn(1) :- aiplayA(1).
playTurn(2) :- aiplayB(2).


playGame :- repeat, nb_getval(turn, X), playModulo(X), A is X+1, nb_setval(turn, A), (victory(X)).