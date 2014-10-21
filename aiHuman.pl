% Dummy AI playing always in the same column (2)
:- module(aiHuman, [play/1]).
play(X) :-  %repeat,							% Would be great to re-ask when the player tried a non valid move
			write("The current grid is :"),nl,
			showGrid,nl,
			write("Please write a column number ended by a dot"),nl,
			read(I), write("You chose "+I),nl,
			%(add(I,X)).
			add(I,X).
