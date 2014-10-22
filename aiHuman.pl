% Fake AI that seems like an AI but really is asking for user input
:- module(aiHuman, [play/1]).
play(X) :-  write("The current grid is :"),nl,
			showGrid,nl,
			write("Please write a column number ended by a dot"),nl,
			read(I), write("You chose "),write(I),nl,
			add(I,X).
