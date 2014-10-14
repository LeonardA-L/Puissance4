% Dummy AI playing always in the same column (2)
:- module(ai2, [play/1]).
play(NbJ):-	random(1,8,Res1),
			placeAToken(Res1,Nbj).
%play(A) :- add(5,A).