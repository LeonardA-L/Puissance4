% Yes
%:- [ai1].
:- use_module(ai1, [play/1 as aiplayA]).

%:- [ai2].
:- use_module(ai2, [play/1 as aiplayB]).

victory(_).

% load AI modules
 playGame(X) :- aiplayA(X), N is (mod(X+1,2)),
	       victory(X),
	       aiplayB(N), K is (mod(N+1,2)),
	       victory(N),
	       playGame(K).


