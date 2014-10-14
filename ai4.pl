% AI playing MinMax
:- module(ai4, [play/1]).
play(NbJ):-	

Puissance 4 à 4 colonnes

coupAJouer(prof,nb,tab)
	si prof == profMax
		retourner 0
	si testV
		retourner 100
	int res;
	modiftab
		res += couAJouer(prof+1,1,tab)
	modiftab
		res += couAJouer(prof+1,2,tab)
	modiftab
		res += couAJouer(prof+1,3,tab)
	modiftab
		res += couAJouer(prof+1,4,tab)
	retourner res;