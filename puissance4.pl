% Load external modules
:- [map].

% Set dynamic variables to be able to do some stats
:- dynamic amountInSet/1.
:- dynamic aWon/1.
:- dynamic bWon/1.

aWon(0).
bWon(0).
amountInSet(-1).


% loading AI modules (called when needed)
chooseA(silly) :- use_module(ai1, [play/1 as aiplayA]).
chooseA(human) :- use_module(aiHuman, [play/1 as aiplayA]).
chooseA(random):- use_module(ai2, [play/1 as aiplayA]).

chooseB(silly) :- use_module(ai1, [play/1 as aiplayB]).
chooseB(human) :- use_module(aiHuman, [play/1 as aiplayB]).
chooseB(random):- use_module(ai2, [play/1 as aiplayB]).

% Loading alignement check
:- use_module(alignment, [isWin/3 as isWin]).


% Set "number of turns" global variable
% Yes, this could have been done with a regular assert
:- nb_setval(turn, 0).
:- nb_setval(game, 0).

listAI :- write('- human'), nl,
		  write('- random'), nl.

config :- write('Hello, and welcome to the Prolog enrichment center.'), nl,
	% Choose AI A
	repeat,
	write('Please choose AI A between :'), nl,
	listAI,
	read(X), (chooseA(X)),
	assert(a(X)),
	% Choose AI B
	repeat,
	write('Please choose AI B between :'), nl,
	listAI,
	read(Y), (chooseB(Y)),
	assert(b(Y)).

% Increment the stats part
incrementWinner(1) :- retract(aWon(R)), A is R+1, assert(aWon(A)).
incrementWinner(2) :- retract(bWon(R)), A is R+1, assert(bWon(A)).


% Test Victory
victory(41) :- write('End of the game, it\'s a TIE !'),nl, showGrid.
victory(X):-
    etat(Map),
    getPlayerSymbol(X, Player),
    pos(Pos),
    isWin(Map, Player, Pos),
    write('victory ! Player '), write(Player), write(" wins after "), write(X), write(" turns."), nl,
	incrementWinner(Player),
    showGrid.


getPlayerSymbol(X, Player):-
    Player is (X mod 2) + 1.

% Don't know why this has to be nested. Computes ID of currently player and calls for him
playModulo(X) :- getPlayerSymbol(X, B), write("player "), write(B), write(" is playing"),nl, playTurn(B).

% Calls play method of the right player module
playTurn(1) :- aiplayA(1).
playTurn(2) :- aiplayB(2).

% Main loop :
% - repeat, ... , (victory). means the "..." part will be repeated until victory is true
% - nb_getval : retrieve the current turn
% - nb_setval : increment the current turn before next one
% - playModulo : ask for a player to play
playGame :- reset, repeat, nb_getval(turn, X), playModulo(X), A is X+1, nb_setval(turn, A), (victory(X)).

% Resets the game before starting a new one
reset :- resetMap, nb_setval(turn, 0).
% Resets the stats before launching a set
resetStats :- retract(aWon(_)), assert(aWon(0)), retract(bWon(_)), assert(bWon(0)).

% The predicates used to launch a set of matches.
writePlayer(Player, Won, Total) :- write('Player '),write(Player),write(' won '), write(Won), write(' matches out of '),write(Total), write('.'),nl.
launchSet(0) :- aWon(Awon), bWon(Bwon), amountInSet(Max), a(A), b(B), write('End of the set, '), writePlayer(A, Awon, Max), writePlayer(B, Bwon, Max).
launchSet(Max) :- X is Max - 1, playGame, launchSet(X).


% ------------- Actually used when one wants to launch a game
startMany(Max) :- config, resetStats, retract(amountInSet(_)), assert(amountInSet(Max)), launchSet(Max), !.
startGame :- config, playGame, !.