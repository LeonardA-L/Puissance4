%%% TESTS %%%
:- use_module('../alignment').

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

map3([0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,
      1,0,0,0,0,0,0,
      1,0,0,0,0,0,0,
      1,0,2,0,0,0,0,
      1,0,2,2,0,0,0]).

getPlayerSymbol(_, 1).

positionToCheck(15).



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
    findDown(42, 42),
    findDown(7, 28).

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


getPos(Row, Col, P):-
    Y is Col - 1,
    P is Y*7+Row.
