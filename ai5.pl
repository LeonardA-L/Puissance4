% Load external modules
:- [map].

% Loading alignement check
:- use_module(alignment, [isWin/3 as isWin]).


play(NbJ, Pos):- etat(Map),findBestMove(Map, Pos, NbJ, Res).

findBestMove(Pos, NbJ, Res):- etat1(Map), PosToFill is Pos-7, findElem(Map, PosToFill, Resultat), getOpponent(Opponent, NbJ), Resultat = 0, isWin(Map, Opponent, PosToFill), Res = PosToFill
						;
				etat(Map), PosToFill is Pos-8, findElem(Map, PosToFill, Resultat), getOpponent(Opponent, NbJ), Resultat = 0, isWin(Map, Opponent, PosToFill), Res = PosToFill
						;
				etat(Map), PosToFill is Pos-6, findElem(Map, PosToFill, Resultat), getOpponent(Opponent, NbJ), Resultat = 0, isWin(Map, Opponent, PosToFill), Res = PosToFill
						;
				etat(Map), PosToFill is Pos-1, findElem(Map, PosToFill, Resultat), getOpponent(Opponent, NbJ), Resultat = 0, isWin(Map, Opponent, PosToFill), Res = PosToFill
						;
				etat(Map), PosToFill is Pos+1, findElem(Map, PosToFill, Resultat), getOpponent(Opponent, NbJ), Resultat = 0, isWin(Map, Opponent, PosToFill), Res = PosToFill
						;
				etat(Map), PosToFill is Pos+6, findElem(Map, PosToFill, Resultat), getOpponent(Opponent, NbJ), Resultat = 0, isWin(Map, Opponent, PosToFill), Res = PosToFill
						;
				etat(Map), PosToFill is Pos+8, findElem(Map, PosToFill, Resultat), getOpponent(Opponent, NbJ), Resultat = 0, isWin(Map, Opponent, PosToFill), Res = PosToFill.

getOpponent(Opponent, CurrentPlayer):- Aux is CurrentPlayer+1, Opponent is (Aux mod 2).

myWin(Opponent, PosToFill):- etat(Map), isWin(Map, Opponent, PosToFill).

%Get the element of a list at position N