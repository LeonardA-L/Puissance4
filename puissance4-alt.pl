% Loading Modules : 
:- [map].
:- use_module('ai1-alt', [play/1 as aiplayA]).
:- use_module('ai2-alt', [play/1 as aiplayB]).

:- use_module(library(random)).

%---------------------------------------

% Test purpose : 

isWin(_, _, _) :- random_between(0, 10, 1).
isNotWin(_, _, _) :- \+isWin(_,_,_).

%-----------------------

% Main loop :

% At first, no winner, we choose 'c' as the current state 
:- dynamic gagnant/1.
gagnant(c).

% Two possibilities : A or B win.
victory:-gagnant(a), write('Joueur A gagne').
victory:-gagnant(b), write('Joueur B gagne').

moveIsOk(Pos) :-
			Pos < 8,
			Pos > 0,
			etat(L),
			findElem(L,Pos,Res),
			Res < 1.


% Check wether player can move the token in the column : if he can, the predicate is false, prolog will try the following. If he can't, predicate is true, writes "bad move" and ask the user for another column.

	%placeAToken(Pos,Player):- \+ moveIsOk(Pos), write('bad move'), aiplayA(Player).

% If last predicate returned false, try adding the token and check if the player won :

	placeAToken(Pos,Player) :- add(Pos,Player), isNotWin(_, _, _).

% if the last one failed too, maybe the user has won, in that case, alert the rest of the soft

	placeAToken(Pos,Player) :- isWin(_, _, _), assert(gagnant(Player)).

% PlayerA makes a move, then you check if he won or not, and if he didn't, you keep going.
%If gagnant(c) is false, it means playerA won : then victory will be true and loop will exit. Also in that case, playTurn will fail after aiplayA(1), which will not affect the last move or the predicate gagnant (because they use "assert")
%If gagnant(c) is true, nothing has changed, A has not won, we keeo going with B. 
% aiplayA(1) calls a predicate in ai1 which then calls to placeAToken here

	playTurn :- aiplayA(1), gagnant(c), aiplayB(2).

% Start a game : loop on playTurn until "\+victory" is false.
	
	playGame :- repeat, playTurn,(\+victory).
