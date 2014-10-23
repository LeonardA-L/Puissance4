%% gigAInton.pl
%% ------------
%% implements an agent for playing Connect Four (Puissance 4)
%% using the minimax algorithm.
%%
%% http://en.wikipedia.org/wiki/Minimax
%%
%% BUGS/NOTES/TODO:
%% - No heuristics implemented, NOT tested so it
%% runs --> This doesn't work at all at the moment. <--

:- module(gigAInton, [play/1]).

:- meta_predicate play(+).

%:- use_module(alignment).
:- use_module(library(lists), [flatten/2]).


play(Player):- etat(State), minimax(State, 5, Player, Column), add(Column, Player).

minimax(State, Depth, MaximizingPlayer, Column):-
    Depth > 2,
    getNextStates(State, MaximizingPlayer, NextStates),
    nextPlayerSymbol(MaximizingPlayer, MinPlayerSymbol),
    enumerate1(NextStates, EnumeratedStates),
    NextDepth is Depth - 1,
    findBestColumn(EnumeratedStates, NextDepth, MaximizingPlayer, MinPlayerSymbol, _, Column).

findBestColumn([],_,_,_,-1000,1).
findBestColumn([[State|Col]|States], Depth, MaxPlayerSymbol, MinPlayerSymbol, Value, Col):-
    minimax_(State, Depth, MaxPlayerSymbol, MinPlayerSymbol, minPlayer, Value),
    findBestColumn(States, Depth, MaxPlayerSymbol, MinPlayerSymbol, PrevMaxValue, _),
    Value >= PrevMaxValue.
findBestColumn(_, _, _, _, _, _).


minimax_(State, 0, MaxPlayerSymbol, _, maxPlayer, Val):-
    heuristic(State, MaxPlayerSymbol, Val).

minimax_(State, 0, _, MinPlayerSymbol, minPlayer, Val):-
    heuristic(State, MinPlayerSymbol, V1),
    Val is -V1.

minimax_(State, Depth, MaxPlayerSymbol, MinPlayerSymbol, maxPlayer, Value):-
    getNextStates(State, MaxPlayerSymbol, NextStates),
    NewDepth is Depth - 1,
    max_children(NextStates, NewDepth, MaxPlayerSymbol, MinPlayerSymbol, Value).

minimax_(State, Depth, MaxPlayerSymbol, MinPlayerSymbol, minPlayer, Value):-
    getNextStates(State, MinPlayerSymbol, NextStates),
    NewDepth is Depth - 1,
    min_children(NextStates, NewDepth, MaxPlayerSymbol, MinPlayerSymbol, Value).

max_children([],_,_,_,_,_,-1000).
max_children([State|States], NewDepth, MaxPlayerSymbol, MinPlayerSymbol, Value):-
    minimax_(State, NewDepth, MaxPlayerSymbol, MinPlayerSymbol, V1),
    max_children(States, NewDepth, MaxPlayerSymbol, MinPlayerSymbol, V2),
    Value is max(V1, V2).

min_children([],_,_,_,_,_,1000).
min_children([State|States], NewDepth, MaxPlayerSymbol, MinPlayerSymbol, Value):-
    minimax_(State, NewDepth, MaxPlayerSymbol, MinPlayerSymbol, V1),
    min_children(States, NewDepth, MaxPlayerSymbol, MinPlayerSymbol, V2),
    Value is min(V1, V2).


%% heuristic/1
% should return a value between -1000 and 1000,
% as these are seen as (-/+) infinity.

heuristic(_State, Value):-
    Value = 10.


% Helper to enumerate a list
enumerate1(L1, L2):-
    enumerate(L1, 1, L2).

enumerate([], _, []).
enumerate([H|T], I, [[H, I]|R]):-
    J is I + 1,
    enumerate(T, J, R). 


                
           
% Returns a list of the next nodes in the search tree from State
getNextStates(State, CurrentPlayer, NextStates):-
    nextPlayerSymbol(CurrentPlayer, Player),
    addToColumn(State, 1, Player, R1),
    addToColumn(State, 2, Player, R2),
    addToColumn(State, 3, Player, R3),
    addToColumn(State, 4, Player, R4),
    addToColumn(State, 5, Player, R5),
    addToColumn(State, 6, Player, R6),
    addToColumn(State, 7, Player, R7),
    flatten([R1, R2, R3, R4, R5, R6, R7], NextStates).


% Copy from map.pl but slightly modified
addToColumn(State, Col, Player, Result):-
    Col < 8,
    Col > 0,
    Player > 0,
    Player < 3,
    findElem(State, Col, Res),
    Res < 1,
    findPos(State, Col, 0, Pos),
    Pos1 is Pos - 7,
    replace(State, Pos1, Player, Result).
addToColumn(_,_,_,[]).


nextPlayerSymbol(CurrentPlayer, NextPlayer):-
    NextPlayer is ((CurrentPlayer + 1) mod 2) + 1.
