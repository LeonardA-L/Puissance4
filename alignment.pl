:- module(alignment, [isWin/3
                      isNotAWin/3,
                      findUp/2,
                      findDown/2,
                      findLeft/2,
                      findRight/2,
                      findUpLeft/2,
                      findUpRight/2,
                      findDownLeft/2,
                      findDownRight/2,
                      findUpLeftDiag/2,
                      findUpRightDiag/2,
                      findDownLeftDiag/2,
                      findDownRightDiag/2,
                      generateCombinations/3,
                      checkCombinations/2.]).
:- use_module(library(lists), [nth1/3]).


:- meta_predicate isWin(+, +, +),
    isNotAWin(+, +, +),
    findUp(+, -),
    findDown(+, -),
    findLeft(+, -),
    findRight(+, -),
    findUpLeft(+, -),
    findUpRight(+, -),
    findDownLeft(+, -),
    findDownRight(+, -),
    findUpLeftDiag(+, -),
    findUpRightDiag(+, -),
    findDownLeftDiag(+, -),
    findDownRightDiag(+, -),
    generateCombinations(+, +, -),
    checkCombinations(+, +).

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

% helper
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
% names of the helper predicates.


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



