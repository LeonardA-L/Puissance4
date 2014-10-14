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

generateHorizontal(Map, Pos, Combinations).

generateDiagonals(Map, Pos, Combinations).



%% Find

% Finds the uppermost position possible of Pos with
up(Pos, NewPos):-
    up4(Pos, 0, NewPos).
up4(Pos, 3, Pos).
up4(Pos, Iter, NewPos):-
    Pos >= 8 *->
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

left(Pos, NewPos):-
    left4(Pos, 0, NewPos).

left4(Pos, 3, Pos).
left4(Pos, Iter, NewPos) :-
    Rem is Pos mod 7,
    (Rem \== 1)*->
    (NextPos is Pos - 1,
    NextIter is Iter + 1,
    left4(NextPos, NextIter, NewPos));
    NewPos = Pos.

right(Pos, NewPos):-
    right4(Pos, 0, NewPos).

right4(Pos, 3, Pos).
right4(Pos, Iter, NewPos) :-
    Rem is Pos mod 7,
    (Rem \== 0)*->
    (NextPos is Pos + 1,
    NextIter is Iter + 1,
    right4(NextPos, NextIter, NewPos));
    NewPos = Pos.

upLeft(Pos, NewPos):-
    upLeftDiag4(Pos, 0, NewPos).

upLeftDiag4(Pos, 3, Pos).
upLeftDiag4(Pos, Iter, NewPos) :-
    Rem is Pos mod 7,
    (Rem \== 1 , Pos >= 8)*->
    (NextPos is Pos - 8,
    NextIter is Iter + 1,
    upLeftDiag4(NextPos, NextIter, NewPos));
    NewPos = Pos.

upRightDiag(Pos, NewPos):-
    upRightDiag4(Pos, 0, NewPos).

upRightDiag4(Pos, 3, Pos).
upRightDiag4(Pos, Iter, NewPos) :-
    Rem is Pos mod 7,
    (Pos >= 8, Rem \== 0)*->
    (NextPos is Pos - 6,
    NextIter is Iter + 1,
    upRightDiag4(NextPos, NextIter, NewPos));
    NewPos = Pos.

downLeftDiag(Pos, NewPos):-
    downLeftDiag4(Pos, 0, NewPos).

downLeftDiag4(Pos, 3, Pos).
downLeftDiag4(Pos, Iter, NewPos) :-
    Rem is Pos mod 7,
    (Rem \== 1 , Pos =< 35)*->
    (NextPos is Pos + 6,
    NextIter is Iter + 1,
    downLeftDiag4(NextPos, NextIter, NewPos));
    NewPos = Pos.


downRightDiag(Pos, NewPos):-
    downRightDiag4(Pos, 0, NewPos).

downRightDiag4(Pos, 3, Pos).
downRightDiag4(Pos, Iter, NewPos) :-
    Rem is Pos mod 7,
    (Pos =< 34, Rem \== 0)*->
    (NextPos is Pos + 8,
    NextIter is Iter + 1,
    downRightDiag4(NextPos, NextIter, NewPos));
    NewPos = Pos.


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
    up(42, 21),
    up(15, 1).

tdown:-
    down4(42, 42),
    down4(7, 28).

tleft:-
    left(1, 1),
    left(7, 4),
    left(36, 36),
    left(42, 39).

tupleft:-
    upLeftDiag(9, 1),
    upLeftDiag(1, 1),
    upLeftDiag(42, 18).

tupright:-
    upRightDiag(8, 2),
    upRightDiag(14, 14),
    upRightDiag(25, 7).

tdownleft:-
    downLeftDiag(2, 8),
    downLeftDiag(24, 36),
    downLeftDiag(41, 41),
    downLeftDiag(35, 41).

tdownright:-
    downRightDiag(26, 42),
    downRightDiag(1, 25),
    downRightDiag(23, 39),
    downRightDiag(6, 14).


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
