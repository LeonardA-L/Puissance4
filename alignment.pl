:- use_module(library(lists)).


isWin(Map, Player, Pos):-
    \+ isNotAWin(Map, Player, Pos).

isNotAWin(Map, Player, Pos):-
    generateCombinations(Map, Pos, Combinations),
    !,
    \+ checkCombinations(Player, Combinations).

generateCombinations(Map, Pos, Combinations):-
    generateHorizontal(Map, Pos, HorizontalCombinations),
    generateVertical(Map, Pos, VerticalCombinations),
    generateDiagonals(Map, Pos, DiagonalCombinations),
    append(HorizontalCombinations,VerticalCombinations, C1),
    append(C1, DiagonalCombinations, Combinations).



%% Vertical

generateVertical(Map, Pos, Combinations):-
     findUp(Pos, UpPos),
     findDown(Pos, DownPos),
     verticalDelta(UpPos, DownPos, Delta),
     verticalCombinations(Map, UpPos, Delta, Combinations).


verticalDelta(X, Y, Delta):-
    Delta is (Y - X) div 7.

verticalCombinations(_, _, Delta, []):-
    Delta =< 2.
verticalCombinations(Map, Pos, Delta, [Combination|Combinations]):-
    verticalCombination(Map, Pos, Combination),
    NextPos is Pos + 7,
    NextDelta is Delta - 1,
    verticalCombinations(Map, NextPos, NextDelta, Combinations).

verticalCombination(Map, Pos, Combination):-
    getCombination(Map, Pos, 7, Combination).
    
%% Horizontal

generateHorizontal(Map, Pos, Combinations):-
    findLeft(Pos, Left),
    findRight(Pos, Right),
    horizontalDelta(Left, Right, Delta),
    horizontalCombinations(Map, Left, Delta, Combinations).


horizontalCombinations(_,_,Delta,[]):-
    Delta =< 2.
horizontalCombinations(Map, Pos, Delta, [Combination|Combinations]):-
    horizontalCombination(Map, Pos, Combination),
    NextPos is Pos + 1,
    NextDelta is Delta - 1,
    horizontalCombinations(Map, NextPos, NextDelta, Combinations).

horizontalCombination(Map, Pos, Combination):-
    getCombination(Map, Pos, 1, Combination).

horizontalDelta(Start, End, Delta):-
    Delta is (End - Start).


%% Diagonals

% The diagonal combinations will always start from up and go down.
% Like 1        3 so => [[1, 2, ..], [2, ..], [3, 4, ..], [4, ..]]
%       2      4
%        ......

diagonalCombinationUpLeft(Map, Pos, Combination):-
    getCombination(Map, Pos, 8, Combination).

diagonalCombinationUpRight(Map, Pos, Combination):-
    getCombination(Map, Pos, 6, Combination).


diagonalDelta(UpPos, DownPos, Delta):-
    A is (UpPos - 1) mod 7,
    B is (DownPos - 1) mod 7,
    maxmin(A, B, Y, X),
    horizontalDelta(X, Y, Delta).

maxmin(A, B, A, B):-
    A >= B.
maxmin(A, B, B, A).

    

generateDiagonals(Map, Pos, Combinations):-
    generateDiagonalsUpLeft(Map, Pos, C1),
    generateDiagonalsUpRight(Map, Pos, C2),
    append(C1, C2, Combinations).

generateDiagonalsUpLeft(Map, Pos, Combinations):-
    findUpLeft(Pos, UpLeft),
    findDownRight(Pos, DownRight),
    diagonalDelta(UpLeft, DownRight, Delta),
    diagonalCombinationsUpLeft(Map, UpLeft, Delta, Combinations).

diagonalCombinationsUpLeft(_,_,Delta,[]):-
    Delta =< 2.
diagonalCombinationsUpLeft(Map, Pos, Delta, [Combination|Combinations]):-
    diagonalCombinationUpLeft(Map, Pos, Combination),
    NextDelta is Delta - 1,
    NextPos is Pos + 8,
    diagonalCombinationsUpLeft(Map, NextPos, NextDelta, Combinations).


generateDiagonalsUpRight(Map, Pos, Combinations):-
    findUpRight(Pos, UpRight),
    findDownLeft(Pos, DownLeft),
    diagonalDelta(UpRight, DownLeft, Delta),
    diagonalCombinationsUpRight(Map, UpRight, Delta, Combinations).

diagonalCombinationsUpRight(_, _, Delta, []):-
    Delta =< 2.
diagonalCombinationsUpRight(Map, Pos, Delta, [Combination|Combinations]):-
    diagonalCombinationUpRight(Map, Pos, Combination),
    NextDelta is Delta - 1,
    NextPos is Pos + 6,
    diagonalCombinationsUpRight(Map, NextPos, NextDelta, Combinations).
    
    


% Helper

getCombination(Map, Pos, Offset, [X1, X2, X3, X4]):-
    I is Pos + Offset,
    J is I + Offset,
    K is J + Offset,
    nth1(Pos, Map, X1),
    nth1(I, Map, X2),
    nth1(J, Map, X3),
    nth1(K, Map, X4).



%% Find
% The 'findX' predicates finds the position furthest away possible in
% a maximum range of four steps away. Hence the appended '4' to the
% functors of the helper predicates.


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

findUpRight(Pos, NewPos):-
    upRightDiag4(Pos, 0, NewPos).

upRightDiag4(Pos, 3, Pos).
upRightDiag4(Pos, Iter, NewPos) :-
    Rem is Pos mod 7,
    (Pos >= 8, Rem \== 0)*->
    (NextPos is Pos - 6,
    NextIter is Iter + 1,
    upRightDiag4(NextPos, NextIter, NewPos));
    NewPos = Pos.

findDownLeft(Pos, NewPos):-
    downLeftDiag4(Pos, 0, NewPos).

downLeftDiag4(Pos, 3, Pos).
downLeftDiag4(Pos, Iter, NewPos) :-
    Rem is Pos mod 7,
    (Rem \== 1 , Pos =< 35)*->
    (NextPos is Pos + 6,
    NextIter is Iter + 1,
    downLeftDiag4(NextPos, NextIter, NewPos));
    NewPos = Pos.


findDownRight(Pos, NewPos):-
    downRightDiag4(Pos, 0, NewPos).

downRightDiag4(Pos, 3, Pos).
downRightDiag4(Pos, Iter, NewPos):-
    Rem is Pos mod 7,
    (Pos =< 34, Rem \== 0)*->
    (NextPos is Pos + 8,
    NextIter is Iter + 1,
    downRightDiag4(NextPos, NextIter, NewPos));
    NewPos = Pos.


% This predicate took about 1h 50 min to get right...
checkCombinations(Player, Combinations):-
    \+ checkCombinations_(Player, Combinations).

checkCombinations_(_, []).
checkCombinations_(Player, [[Player, Player, Player, Player]| _]):-
    !, fail.
checkCombinations_(Player, [[_,_,_,_]|CS]) :-
    checkCombinations_(Player, CS).



getPos(Row, Col, P):-
    Y is Col - 1,
    P is Y*7+Row.


%%% TESTS %%%


map1([1,0,0,1,0,0,7,            % Test map
      0,2,0,2,0,6,0,
      0,0,3,3,5,0,0,
      1,2,3,4,5,6,7,            % '4' is on pos 25
      0,0,3,5,5,0,0,
      0,2,0,6,0,6,0]).

map2([0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,
      0,2,0,0,0,0,0,
      0,2,2,0,0,0,0,
      0,1,1,2,1,0,0,
      0,1,1,1,2,1,0]).



%% Test combination generation

testGenerateCombinations(X):-
    map2(M),
    generateCombinations(M, 16, X).

% Diagonals

testGenerateDiagonals:-
    map1(M),
    generateDiagonals(M, 25, L),
    sort(L, S),
    sort([[1,2,3,4], [2,3,4,5], [3,4,5,6], [7,6,5,4], [6,5,4,3], [5,4,3,2]], S).

testDiagonalDelta:-
    diagonalDelta(1, 9, 1),     % upleft - downright
    diagonalDelta(1, 25, 3),    % upleft - downright
    diagonalDelta(7, 25, 3),    % upright - downleft
    diagonalDelta(14, 26, 2).   % upright - downleft


testDiagonalCombinationUpLeft:-
    map1(M),
    diagonalCombinationUpLeft(M, 1, [1, 2, 3, 4]),
    diagonalCombinationUpLeft(M, 16, [0, 3, 5, 0]).

testDiagonalCombinationUpRight:-
    map1(M),
    diagonalCombinationUpRight(M, 7, [7, 6, 5, 4]),
    diagonalCombinationUpRight(M, 21, [0, 6, 5, 6]).



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
    verticalCombination(M, 4, [1, 2, 3, 4]),
    verticalCombination(M, 20, [0, 6, 0, 6]).

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
    horizontalCombination(M, 22, [1, 2, 3, 4]),
    horizontalCombination(M, 16, [0, 3, 3, 5]).

testAllGenerationPredicates:-
    testHorizontalCombination,
    testHorizontalDelta,
    testGenerateHorizontal,
    testVerticalCombination,
    testVerticalDelta,
    testGenerateVertical,
    testDiagonalCombinationUpRight,
    testDiagonalCombinationUpLeft,
    testDiagonalDelta,
    testGenerateDiagonals.


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

tright:-
    findRight(1, 7),
    findRight(7, 7),
    findRight(25, 28),
    findRight(41, 42).

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

% MEGA TEST for check combinations
mt:-
    tcc,
    tcc2,
    tcc3,
    tcc4,
    tcc5,
    tcc6,
    tcc7.


%% Tests for isWin and isNotWin
t1:-
    map1(Map),
    getPos(3, 3, Pos),
    isNotAWin(Map, 'R', Pos).

t2:-
    map2(Map),
    isWin(Map, 2, 16).

t3:-
    map2(Map),
    isNotAWin(Map, 1, 16).

go:-
    consult('alignment.pl').
