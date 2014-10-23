% Load external modules
:- [map].
:- use_module(alignment, [isWin/3 as isWin]).

play(Player):- etat(Map), pos(Pos), findBestMove(Map, Pos, Player, Res).

% etatMapAux([]).

findBestMove(Pos, Player, Res):- etat(Map), assert(mapCpy(Map)), PosToFill is Pos-7, getOpponent(Player, Opponent), Col is PosToFill div 7, add(Col, Player), isWin(Map, Opponent, PosToFill), Res is PosToFill, retract(etat(_)), mapCpy(X), assert(etat(X))
						;
				etat(Map), assert(mapCpy(Map)), PosToFill is Pos-8, getOpponent(Player, Opponent), COl is PosToFill div 7, add(Col, Player), isWin(Map, Opponent, PosToFill), Res is PosToFill, retract(etat(_)), mapCpy(X)
						;
				etat(Map), assert(mapCpy(Map)), PosToFill is Pos-6, getOpponent(Player, Opponent), COl is PosToFill div 7, add(Col, Player), isWin(Map, Opponent, PosToFill), Res is PosToFill, retract(etat(_)), mapCpy(X)
						;
				etat(Map), assert(mapCpy(Map)), PosToFill is Pos-1, getOpponent(Player, Opponent), COl is PosToFill div 7, add(Col, Player), isWin(Map, Opponent, PosToFill), Res is PosToFill, retract(etat(_)), mapCpy(X)
						;
				etat(Map), assert(mapCpy(Map)), PosToFill is Pos+1, getOpponent(Player, Opponent), COl is PosToFill div 7, add(Col, Player), isWin(Map, Opponent, PosToFill), Res is PosToFill, retract(etat(_)), mapCpy(X)
						;
				etat(Map), assert(mapCpy(Map)), PosToFill is Pos+6, getOpponent(Player, Opponent), COl is PosToFill div 7, add(Col, Player), isWin(Map, Opponent, PosToFill), Res is PosToFill, retract(etat(_)), mapCpy(X)
						;
				etat(Map), assert(mapCpy(Map)), PosToFill is Pos+8, getOpponent(Player, Opponent), COl is PosToFill div 7, add(Col, Player), isWin(Map, Opponent, PosToFill), Res is PosToFill, retract(etat(_)), mapCpy(X).


getOpponent(X, Player):- Player is (X+1) mod 2.


%getOpponent(Opponent, CurrentPlayer):- Aux is CurrentPlayer+1, Opponent is (Aux mod 2).

myWin(Opponent, PosToFill):- etat(Map), isWin(Map, Opponent, PosToFill).

%Get the element of a list at position N
