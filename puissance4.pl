% Yes
%:- [ai1].
:- [map].
:- use_module(ai1, [play/1 as aiplayA]).
:- use_module(ai2, [play/1 as aiplayB]).

victory(X):-X<10.

% not stopping for the moment.
% load AI modules
playGame(X) :- A is mod(X,2)+1, aiplayA(A), N is X+1,
	       victory(X),
	       B is mod(N,2)+1, aiplayB(B), K is N+1,
	       victory(N),
	       playGame(K).
