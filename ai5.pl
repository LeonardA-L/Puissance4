% Load external modules
:- [map].

% Loading alignement check
:- use_module(alignment, [isWin/3 as isWin]).


play(NbJ, Pos):- etat(Map),findBestMove(Map, Pos, NbJ, Res).

findBestMove(Pos, X, Res):- etat(Map), PosToFill is Pos-7, getPlayerSymbol(X, Player), pos(PosToFill), isWin(Map, Player, PosToFill), Res is PosToFill
						;
				etat(Map), PosToFill is Pos-8, getPlayerSymbol(X, Player), pos(PosToFill), isWin(Map, Player, PosToFill), Res is PosToFill
						;
				etat(Map), PosToFill is Pos-6, getPlayerSymbol(X, Player), pos(PosToFill), isWin(Map, Player, PosToFill), Res is PosToFill
						;
				etat(Map), PosToFill is Pos-1, getPlayerSymbol(X, Player), pos(PosToFill), isWin(Map, Player, PosToFill), Res is PosToFill
						;
				etat(Map), PosToFill is Pos+1, getPlayerSymbol(X, Player), pos(PosToFill), isWin(Map, Player, PosToFill), Res is PosToFill
						;
				etat(Map), PosToFill is Pos+6, getPlayerSymbol(X, Player), pos(PosToFill), isWin(Map, Player, PosToFill), Res is PosToFill
						;
				etat(Map), PosToFill is Pos+8, getPlayerSymbol(X, Player), pos(PosToFill), isWin(Map, Player, PosToFill), Res is PosToFill.


getPlayerSymbol(X, Player):- Player is (X+1) mod 2.


getOpponent(Opponent, CurrentPlayer):- Aux is CurrentPlayer+1, Opponent is (Aux mod 2).

myWin(Opponent, PosToFill):- etat(Map), isWin(Map, Opponent, PosToFill).

%Get the element of a list at position N