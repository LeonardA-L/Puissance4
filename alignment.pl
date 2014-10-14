:-use_module(library(lists)).

isWin(Map, Player, Pos):-
    \+ isNotAWin(Map, Player, Pos).

isNotAWin(Map, Player, Pos):-
    generateCombinations(Map, Pos, Combinations),
    checkCombinations(Player, Combinations).

%% TODO
generateCombinations(Map, Pos, Combinations):-
    generateHorizontal(Map, Pos, HorizontalCombs),
    generateVertical(Map, Pos, VerticalCombs),
    generateDiagonals(Map, Pos, DiagonalCombs),
    append(HorizontalCombs, Vertical, Temp),
    append(Temp, DiagonalCombs, Combinations).

generateVertical(Map, Pos, Combinations):-
    up(N, NewPos).


%% Find

% Finds the uppermost position possible of Pos with
up(Pos, NewPos):-
    up4(Pos, 0, NewPos).
up4(Pos, 3, Pos).
up4(Pos, Iter, NewPos):-
    Pos >= 7 *->
    (NextPos is Pos - 7,
    NextIter is Iter + 1,
    up4(NextPos, NextIter, NewPos));
    NewPos = Pos.

down(Pos, NewPos):-
    down4(Pos, 0, NewPos).

down4(Pos, 3, Pos).
down4(Pos, Iter, NewPos):-
    Pos =< 35 *->
    (NextPos is Pos + 7,
    NextIter is Iter + 1,
    down4(NextPos, NextIter, NewPos));
    NewPos = Pos.

generateVertical(Map, Pos, Combinations).

generateDiagonals(Map, Pos, Combinations).


% +1h 50 min :'(
checkCombinations(Player, Combinations):-
    \+ checkCombinations_(Player, Combinations).

checkCombinations_(_, []).
checkCombinations_(Player, [[Player, Player, Player, Player]| _]) :-
    !, fail.
checkCombinations_(Player, [[_,_,_,_]|CS]) :-
    checkCombinations_(Player, CS).



getPos(Row, Col, P):-
    P is Col*7+Row.

map1([0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,
      0,0,0,0,0,0,0]).


%%% TESTS %%%

%% Test directions
tup:-
    up4(42, 0, 21),
    up4(15, 0, 1).

tdown:-
    down4(42, 0, 42),
    down4(7, 0 , 28).


%% Test checkCombinations
tcc:-
    \+ checkCombinations('r', [[0,0,0,0],[-2,34,4,4],[43,6,5,2]]).

tcc2:-
    checkCombinations('r', [[0,0,0,0],['r','r','r','r'],[43,6,5,2]]).

tcc3:-
    checkCombinations('r', [[0,0,0,0],[-2,34,4,4],[43,6,5,2],['r','r','r','r'],[43,6,5,2],[-2,34,4,4],[43,6,5,2]]).



tcc4:-
    checkCombinations('r', [['r','r','r','r'],[43,6,5,2],[0,0,0,0],[-2,34,4,4],[43,6,5,2]]).

tcc5:-
    checkCombinations('r', [[43,6,5,2],[0,0,0,0],[-2,34,4,4],[0,0,0,0],['r','r','r','r']]).

tcc6:-
    \+ checkCombinations('r', [[0,0,0,0],[43,6,5,2],[0,0,0,0],[-2,34,4,4],[43,6,5,2]]).

tcc7:-
    checkCombinations('r', [[0,0,0,0],[43,6,5,2],[0,0,0,0],[-2,34,4,4],['r','r','r','r'],[43,6,5,2],[0,0,0,0],[-2,34,4,4],[43,6,5,2]]).

% MEGA TEST
mt:-
    tcc,
    tcc2,
    tcc3,
    tcc4,
    tcc5,
    tcc6,
    tcc7.

t1:-
    map1(Map),
    getPos(3, 3, Pos),
    isNotAWin(Map, 'R', Pos).

go:-
    consult('alignment.pl').
