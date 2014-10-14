:- use_module(library(lists)).


isWin(Map, Player, Pos):-
    \+ isNotAWin(Map, Player, Pos).

isNotAWin(Map, Player, Pos):-
    generateCombinations(Map, Pos, Combinations),
    checkCombinations(Player, Combinations).

generateCombinations(Map, Pos, Combinations):-
    generateHorizontal(Map, Pos, HorizontalCombinations),
    generateVertical(Map, Pos, VerticalCombinations),
    generateDiagonals(Map, Pos, DiagonalCombinations),
    append(HorizontalCombinations, VerticalCombinations, Temp),
    append(Temp, DiagonalCombinations, Combinations).


%% Vertical

generateVertical(Map, Pos, Combinations):-
     findUp(Pos, UpPos),
     findDown(Pos, DownPos),
     verticalDelta(UpPos, DownPos, Delta),
     generateVertical_(Map, UpPos, Delta, Combinations).

generateVertical_(_, _, Delta, []):-
    Delta =< 3.
generateVertical_(Map, Start, Delta, Combinations):-
     verticalCombinations(Map, Start, Delta, Combinations).

verticalDelta(X, Y, Delta):-
    Delta is (Y - X) div 7.

verticalCombinations(_,_,2, []).
verticalCombinations(Map, Pos, Delta, [Combination|Combinations]):-
    verticalCombination(Map, Pos, Combination),
    NextPos is Pos + 7,
    NextDelta is Delta - 1,
    verticalCombinations(Map, NextPos, NextDelta, Combinations).

verticalCombination(Map, Pos, [X1, X2, X3, X4]):-
    I is Pos + 7,
    J is Pos + 14,
    K is Pos + 21,
    nth1(Pos, Map, X1),
    nth1(I, Map, X2),
    nth1(J, Map, X3),
    nth1(K, Map, X4).

%% Horizontal

generateHorizontal(Map, Pos, Combinations):-
    findLeft(Pos, Left),
    findRight(Pos, Right),
    horizontalDelta(Left, Right, Delta),
    generateHorizontal_(Map, Left, Delta, Combinations).

generateHorizontal_(_,_, Delta,[]):-
    Delta =< 3.
generateHorizontal_(Map, Pos, Delta, Combinations):-
    horizontalCombinations(Map, Pos, Delta, Combinations).

horizontalCombinations(_,_,2,[]).
horizontalCombinations(Map, Pos, Delta, [Combination|Combinations]):-
    horizontalCombination(Map, Pos, Combination),
    NextPos is Pos + 1,
    NextDelta is Delta - 1,
    horizontalCombinations(Map, NextPos, NextDelta, Combinations).

horizontalCombination(Map, Pos, [X1, X2, X3, X4]):-
    I is Pos + 1,
    J is Pos + 2,
    K is Pos + 3,
    nth1(Pos, Map, X1),
    nth1(I, Map, X2),
    nth1(J, Map, X3),
    nth1(K, Map, X4).
    

horizontalDelta(Start, End, Delta):-
    Delta is (End - Start).


%% Diagonal

generateDiagonals(Map, Pos, Combinations).



%% Find

% Finds the uppermost position possible of Pos with
findUp(Pos, NewPos):-
    up4(Pos, 0, NewPos).
up4(Pos, 3, Pos).
up4(Pos, Iter, NewPos):-
    Pos >= 8 *->
    (NextPos is Pos - 7,
    NextIter is Iter + 1,
    up4(NextPos, NextIter, NewPos));
    NewPos = Pos.

findDown(Pos, NewPos):-
    down4(Pos, 0, NewPos).

down4(Pos, 3, Pos).
down4(Pos, Iter, NewPos):-
    Pos =< 35 *->
    (NextPos is Pos + 7,
    NextIter is Iter + 1,
    down4(NextPos, NextIter, NewPos));
    NewPos = Pos.

findLeft(Pos, NewPos):-
    left4(Pos, 0, NewPos).

left4(Pos, 3, Pos).
left4(Pos, Iter, NewPos) :-
    Rem is Pos mod 7,
    (Rem \== 1)*->
    (NextPos is Pos - 1,
    NextIter is Iter + 1,
    left4(NextPos, NextIter, NewPos));
    NewPos = Pos.

findRight(Pos, NewPos):-
    right4(Pos, 0, NewPos).

right4(Pos, 3, Pos).
right4(Pos, Iter, NewPos) :-
    Rem is Pos mod 7,
    (Rem \== 0)*->
    (NextPos is Pos + 1,
    NextIter is Iter + 1,
    right4(NextPos, NextIter, NewPos));
    NewPos = Pos.

findUpLeft(Pos, NewPos):-
    upLeftDiag4(Pos, 0, NewPos).

upLeftDiag4(Pos, 3, Pos).
upLeftDiag4(Pos, Iter, NewPos) :-
    Rem is Pos mod 7,
    (Rem \== 1 , Pos >= 8)*->
    (NextPos is Pos - 8,
    NextIter is Iter + 1,
    upLeftDiag4(NextPos, NextIter, NewPos));
    NewPos = Pos.

findUpRightDiag(Pos, NewPos):-
    upRightDiag4(Pos, 0, NewPos).

upRightDiag4(Pos, 3, Pos).
upRightDiag4(Pos, Iter, NewPos) :-
    Rem is Pos mod 7,
    (Pos >= 8, Rem \== 0)*->
    (NextPos is Pos - 6,
    NextIter is Iter + 1,
    upRightDiag4(NextPos, NextIter, NewPos));
    NewPos = Pos.

findDownLeftDiag(Pos, NewPos):-
    downLeftDiag4(Pos, 0, NewPos).

downLeftDiag4(Pos, 3, Pos).
downLeftDiag4(Pos, Iter, NewPos) :-
    Rem is Pos mod 7,
    (Rem \== 1 , Pos =< 35)*->
    (NextPos is Pos + 6,
    NextIter is Iter + 1,
    downLeftDiag4(NextPos, NextIter, NewPos));
    NewPos = Pos.


findDownRightDiag(Pos, NewPos):-
    downRightDiag4(Pos, 0, NewPos).

downRightDiag4(Pos, 3, Pos).
downRightDiag4(Pos, Iter, NewPos):-
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
checkCombinations_(Player, [[Player, Player, Player, Player]| _]):-
    !, fail.
checkCombinations_(Player, [[_,_,_,_]|CS]) :-
    checkCombinations_(Player, CS).



getPos(Row, Col, P):-
    P is Col*7+Row.


%%% TESTS %%%


                                
map1([1,0,0,1,0,0,7,            % Test map
      0,2,0,2,0,6,0,
      0,0,3,3,5,0,0,
      1,2,3,4,5,6,7,            % '4' is on pos 25
      0,0,3,5,5,0,0,
      0,2,0,6,0,6,0]).


%% Test combination generation

% Vertical
testGenerateVertical:-
    map1(M),
    generateVertical(M, 25, [[1, 2, 3, 4], [2, 3, 4, 5], [3, 4, 5, 6]]),
    generateVertical(M, 9, [[0, 2, 0, 2], [2, 0, 2, 0], [0, 2, 0, 2]]).

testVerticalDelta:-
    verticalDelta(4, 39, 5),
    verticalDelta(1, 8, 1),
    verticalDelta(1, 22, 3),
    verticalDelta(7, 42, 5).

testVerticalCombination:-
    map1(M),
    verticalCombination(M, 4, 25, [1, 2, 3, 4]),
    verticalCombination(M, 20, 41, [0, 6, 0, 6]).

% Horizontal
testGenerateHorizontal:-
    map1(M),
    generateHorizontal(M, 25, [[1,2,3,4],[2,3,4,5],[3,4,5,6],[4,5,6,7]]),
    generateHorizontal(M, 16, [[0,0,3,3],[0,3,3,5],[3,3,5,0],[3,5,0,0]]).

tg2(X):-
    map1(M),
    generateHorizontal(M, 25, X).

testHorizontalDelta:-
    horizontalDelta(1, 7, 6),
    horizontalDelta(9, 10, 1),
    horizontalDelta(16, 20, 4).

testHorizontalCombination:-
    map1(M),
    horizontalCombination(M, 22, 25, [1, 2, 3, 4]),
    horizontalCombination(M, 16, 19, [0, 3, 3, 5]).

% Diagonal


%% Test directions
tup:-
    findUp(42, 21),
    findUp(15, 1).

tdown:-
    down4(42, 42),
    down4(7, 28).

tleft:-
    findLeft(1, 1),
    findLeft(7, 4),
    findLeft(36, 36),
    findLeft(42, 39).

tupleft:-
    upLeftDiag(9, 1),
    upLeftDiag(1, 1),
    upLeftDiag(42, 18).

tupright:-
    findUpRightDiag(8, 2),
    findUpRightDiag(14, 14),
    findUpRightDiag(25, 7).

tdownleft:-
    findDownLeftDiag(2, 8),
    findDownLeftDiag(24, 36),
    findDownLeftDiag(41, 41),
    findDownLeftDiag(35, 41).

tdownright:-
    findDownRightDiag(26, 42),
    findDownRightDiag(1, 25),
    findDownRightDiag(23, 39),
    findDownRightDiag(6, 14).


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
