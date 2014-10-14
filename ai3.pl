% AI playing random
:- module(ai3, [play/1]).
play(NbJ):-	random(0,8,Res1),
			add(Res1, NbJ).