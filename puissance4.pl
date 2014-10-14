% Yes
%:- [ai1].
:- use_module(ai1, [play/1 as aiplayA]).

%:- [ai2].
:- use_module(ai2, [play/1 as aiplayB]).

victory(_).

% load AI modules
playGame(X) :- aiplayA(mod(X,2)+1), N is X+1,
	       victory(X),
	       aiplayB(mod(N,2)+1), K is N+1,
	       victory(N),
	       playGame(K).


