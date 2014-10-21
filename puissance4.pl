% Load external modules
:- [map].
%:- use_module(ai1, [play/1 as aiplayA]).
:- use_module(aiHuman, [play/1 as aiplayA]).
:- use_module(ai2, [play/1 as aiplayB]).
:- use_module(alignment, [isWin/3 as isWin]).


% Set "number of turns" global variable
:- nb_setval(turn, 0).

% Test Victory
victory(40) :- write('End of the game, it\'s a TIE !').
victory(X):- X>60, write('victory'),write(X).   % 60 is just something we won't reach. To be replaced with actual victory conditions

% Don't know why this has to be nested. Computes ID of currently player and calls for him
playModulo(X) :- B is (X mod 2)+1, write("player "+B+" is playing"),nl, playTurn(B).

% Calls play method of the right player module
playTurn(1) :- aiplayA(1).
playTurn(2) :- aiplayB(2).

% Main loop :
% - repeat, ... , (victory). means the "..." part will be repeated until victory is true
% - nb_getval : retrieve the current turn
% - nb_setval : increment the current turn before next one
% - playModulo : ask for a player to play
playGame :- repeat, nb_getval(turn, X), playModulo(X), A is X+1, nb_setval(turn, A), (victory(X)).

% Resets the game before starting a new one
reset :- resetMap, 
		nb_setval(turn, 0).

