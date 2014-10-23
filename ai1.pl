% IA playing from left to right
:- module(ai1, [play/1]).
:- dynamic column/1.
column(1).
play(X) :- retract(column(A)), B is ((A) mod 7)+1, assert(column(B)), add(A,X).
