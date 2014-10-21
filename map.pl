use_module(library(random)).
%module(UI, showGrid).

:- dynamic etat/1.

etat([0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]).



affich:-write('Affichage\n'),
		etat(L),
		nth0(0, L, E),
		write(E),
		write('\nFin affichage\n').

%Si un jeton existe dans une colonne, on peut ajouter encore un
add(Col, NbP):-write('Modification...'),nl,
		Col < 8,
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
		write('\nModifié\n'),!.

reset:- retract(etat(_)),
		assert(etat([0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0])).

salut(W):- X is W mod 7, X == 6, write(' |\n'),!.
salut(_).

findPos(_,Col,1,PosFinal):-PosFinal = Col.
findPos(_,Col,2,PosFinal):-PosFinal = Col.
findPos(_,Col,-1,PosFinal):-PosFinal is Col.
findPos(List,Col,_,PosFinal):- Position1 is Col+7, findElem(List,Position1,Resultat), findPos(List,Position1,Resultat,PosFinal).

replace([_|T], 1, X, [X|T]).
replace([H|T], I, X, [H|R]):- I > 0, NI is I-1, replace(T, NI, X, R), !.
replace(L, _, _, L).

findElem([Element | _], 1, Resultat):-Resultat = Element.
findElem([],N, Resultat):- N > 0, Resultat = -1.
findElem([_|List],N, Resultat) :- N1 is N-1, findElem(List,N1,Resultat).

showGrid:-	etat(L),
			length(L,Taille),
			TailleN is Taille - 1,
			forall(between(0,TailleN,I),
				(nth0(I, L, E),
				write(' | '),
				write(E),
				salut(I))
			).
