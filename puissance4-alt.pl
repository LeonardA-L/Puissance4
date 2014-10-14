% Loading Modules : 
:- [map].
:- use_module('ai1-alt', [play/1 as aiplayA]).
:- use_module('ai2-alt', [play/1 as aiplayB]).

:- use_module(library(random)).

%---------------------------------------

% Test purpose : 

isWin :- random_between(0,10,1).
isNotWin :- \+isWin.

%-----------------------

% Main loop :

% At first, no winner, we choose 'c' as the current state 
:- dynamic gagnant/1.
gagnant(pasDeGagnant).

% Two possibilities : A or B win.
victory:-gagnant(joueurA), write('Joueur A gagne\n').
victory:-gagnant(joueurB), write('Joueur B gagne\n').

% check if the move the player wanna do is correct :
moveIsOk(Pos) :-
			Pos < 8,
			Pos > 0,
			etat(L),
			findElem(L,Pos,Res),
			Res < 1.

% in case the move was not valid, replay.
replay(1) :- aiplayA(1).
replay(2) :- aiplayB(2).

% change gagnant(pasDeGagnant) to set a winner
changeWinner(1):- retract(gagnant(pasDeGagnant)), assert(gagnant(joueurA)).
changeWinner(2):- retract(gagnant(pasDeGagnant)), assert(gagnant(joueurB)).

placeAToken(Pos,Player):- write('In 1st placeAToken\n'), 
						  \+moveIsOk(Pos), 
						  write('bad move\n'), 
						  replay(Player),
						  write('Out of 1st placeAToken\n').

placeAToken(Pos,Player):- write('In 2nd placeAToken\n'),
						  write(Player),
						  write('\n'), 
						  write(Pos),
						  write('\n'), 
						  add(Pos,Player), 
						  isNotWin,
						  write('Out of 2nd placeAToken\n').				 

placeAToken(_,Player) :-write('In 3rd placeAToken\n'), 
						  changeWinner(Player),
						  write('Out of 3rd placeAToken\n').


% PlayerA makes a move, then you check if he won or not, and if he didn't, you keep going.
% If gagnant(c) is false, it means playerA won : then victory will be true and loop will exit. 
% Also in that case, playTurn will fail after aiplayA(1), which will not affect the 
% last move or the predicate gagnant (because they use "assert")
% If gagnant(c) is true, nothing has changed, A has not won, we keeo going with B. 
% aiplayA(1) calls a predicate in ai1 which then calls to placeAToken here

	playTurn :- aiplayA(1), aiplayB(2).

% Start a game : loop on playTurn until "\+victory" is false.
	
	%playGame :- repeat,(victory), playTurn.

	playGame :- playTurn, victory.
	playGame :- write('Game ended, player won').
