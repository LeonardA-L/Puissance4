% Yes
%:- [ai1].
:- [map].
:- use_module(ai1, [play/1 as aiplayA]).
:- use_module(ai2, [play/1 as aiplayB]).

victory(X):-X>10, write('victory'),write(X).

% load AI modules
playTurn(X) :- mod(X,2) == 0, A is mod(X,2)+1, aiplayA(A).
playTurn(X) :- A is mod(X,2)+1, aiplayB(A).

playGame(X) :- repeat, playTurn(X), X = X+1, (victory(X)).