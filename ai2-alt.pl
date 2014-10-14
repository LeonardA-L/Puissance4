% Dummy AI playing always in the same column (2)
:- module(ai2, [play/1]).
%play(NbJ):-	random(1,7,Res1),		% problem here : if used, error with "not sufficiently instanciated arguments"
%			placeAToken(Res1,Nbj).
play(A) :- placeAToken(5,A).