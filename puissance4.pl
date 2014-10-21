% Load external modules
:- [map].

% loading AI modules
chooseA(silly) :- use_module(ai1, [play/1 as aiplayA]).
chooseA(human) :- use_module(aiHuman, [play/1 as aiplayA]).
chooseA(random):- use_module(ai2, [play/1 as aiplayA]).

chooseB(silly) :- use_module(ai1, [play/1 as aiplayB]).
chooseB(human) :- use_module(aiHuman, [play/1 as aiplayB]).
chooseB(random):- use_module(ai2, [play/1 as aiplayB]).

% Loading alignement check
:- use_module(alignment, [isWin/3 as isWin]).


% Set "number of turns" global variable
:- nb_setval(turn, 0).

listAI :- write('- human'), nl,
		  write('- random'), nl.

startGame :- write('Hello, and welcome to the Prolog enrichment center.'), nl,
	% Choose AI A
	repeat,
	write('Please choose AI A between :'), nl,
	listAI,
	read(X), (chooseA(X)),
	% Choose AI B
	repeat,
	write('Please choose AI B between :'), nl,
	listAI,
	read(Y), (chooseB(Y)),
	playGame.

% Test Victory
victory(40) :- write('End of the game, it\'s a TIE !').
victory(X):-
    etat(Map),
    getPlayerSymbol(X, Player),
    pos(Pos),
    isWin(Map, Player, Pos),
    write('victory'),
    write(X). % 60 is just something we won't reach. To be replaced with actual victory conditions


getPlayerSymbol(X, Player):-
    Player is (X mod 2) + 1.

% Don't know why this has to be nested. Computes ID of currently player and calls for him
playModulo(X) :- getPlayerSymbol(X, B), write("player "+B+" is playing"),nl, playTurn(B).

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

