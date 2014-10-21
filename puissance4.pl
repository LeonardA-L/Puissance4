% Yes
%:- [ai1].
:- [map].
:- use_module(ai1, [play/1 as aiplayA]).
:- use_module(ai2, [play/1 as aiplayB]).
:- use_module(alignment, [isWin/3 as isWin]).
:- nb_setval(turn, 0).

victory(X):- X>10, write('victory'), write(X).

% load AI modules
%playTurn(X) :- A is (X mod 2)+1; (X mod 2) == 0, aiplayA(A) ; (X mod 2) == 1, aiplayB(A).
%playTurn(X) :- A is (X mod 2)+1, aiplayB(A).

playTurn(X) :-  writef('%i',[X mod 2]),((X mod 2) == 0) *->
     (write('oui'),A is (X mod 2)+1,aiplayA(A))
  ; 
    (write('non'),A is (X mod 2)+1,aiplayB(A))
  .

playGame :- repeat, nb_getval(turn, X), playTurn(X), A is X+1, nb_setval(turn, A), (victory(X)).
