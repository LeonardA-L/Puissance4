use_module(library(random)).
%module(UI, showGrid).

:- dynamic etat/1.
:- dynamic pos/1.

% map : list wich stock current game
etat([0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]).

pos(-1).

% test function ??
affich:-write('Affichage\n'),
		etat(L),
		nth0(0, L, E),
		write(E),
		write('\nFin affichage\n').

%Si un jeton existe dans une colonne, on peut ajouter encore un
add(Col, NbP):-	Col < 8,
		Col > 0,
		NbP > 0,
		NbP < 3,
		etat(L),
		findElem(L,Col,Res),
		Res < 1,
		findPos(L,Col,0,Pos),
		Pos1 is Pos-7,
		replace(L,Pos1,NbP,Resultat),
		retract(etat(L)),
		assert(etat(Resultat)),
                retract(pos(_)),
                assert(pos(Pos1)),!.

% set the map to 0
resetMap:- retract(etat(_)),
		assert(etat([0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0])).

% Find the first line where we can play
findPos(_,Col,1,PosFinal):-PosFinal = Col.
findPos(_,Col,2,PosFinal):-PosFinal = Col.
findPos(_,Col,-1,PosFinal):-PosFinal is Col.
findPos(List,Col,_,PosFinal):- Position1 is Col+7, findElem(List,Position1,Resultat), findPos(List,Position1,Resultat,PosFinal).

% replace element in the map
replace([_|T], 1, X, [X|T]).
replace([H|T], I, X, [H|R]):- I > 0, NI is I-1, replace(T, NI, X, R), !.
replace(L, _, _, L).

% find element in the map
findElem([Element | _], 1, Resultat):-Resultat = Element.
findElem([],N, Resultat):- N > 0, Resultat = -1.
findElem([_|List],N, Resultat) :- N1 is N-1, findElem(List,N1,Resultat).

% if last col, go to next line
endLine(W):- X is W mod 7, X == 6, write(' |\n |   |   |   |   |   |   |   |\n'),!.
endLine(_).

% show game's current grid
showGrid:-	write('   1   2   3   4   5   6   7\n  ---------------------------\n'),
			etat(L),
			length(L,Taille),
			TailleN is Taille - 1,
			forall(between(0,TailleN,I),
				(nth0(I, L, E),
				write(' | '),
				write(E),
				endLine(I))
			),
			write('  ---------------------------\n').
