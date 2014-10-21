% Dummy AI playing always in the same column (2)
:- module(ai2, [play/1]).
play(NbJ):-	random(0,8,Res1),
			add(Res1, NbJ).
%play(A) :- add(5,A).
