		%***********************************************************
		%Affichage des domaines de toutes les variables de la grille
		%***********************************************************

:- module(display_sudoku).
:- export(display_grid/2).

:- lib(fd).
        
display_grid(Grid,Verbose) :-
	Verbose = yes,
	dim(Grid,[Nb,Nb]),
	Racine_Nb :: 1..Nb,
	Racine_Nb*Racine_Nb #= Nb,
		%barre supérieure du tableau
	nl,
	write('#'),
	(for(J,1,Nb), param(Nb, Racine_Nb) do
		(for(_,1,Nb) do	 write('#')
		),
	 	Mod is J mod Racine_Nb, 
	 	(Mod = 0 -> write('#');write('|'))
	 ),
	 nl,
	 	%affichage de chaque ligne
	(for(K,1,Nb), param(Nb,Racine_Nb,Grid) do
		Ligne is Grid[K,1..Nb],
		write('#'),
		(foreach(Var, Ligne),for(Col,1,Nb),param(Nb,Racine_Nb) do
				%affichage de chaque variable
			display_var1(Var, Nb),
	 		Mod is Col mod Racine_Nb,
	 		(Mod = 0 -> write('#');write('|'))
	 	),
	 	nl,
			%barres intermediaires
		write('#'),
		Mod is K mod Racine_Nb,
		(Mod = 0 -> Char = '#'; Char='-'),
		(for(J,1,Nb), param(Nb, Racine_Nb,Char) do
			(for(_,1,Nb),param(Char) do	 write(Char)
			),
	 		Mod is J mod Racine_Nb, 
	 		(Mod = 0 -> write('#');write('|'))
	 	),
	 	nl
	 ).


display_grid(Grid,Verbose) :-
	Verbose = no,
	dim(Grid,[Nb,Nb]),
	Racine_Nb :: 1..Nb,
	Racine_Nb*Racine_Nb #= Nb,
		%barre supérieure du tableau
	nl,
	write('#'),
	(for(J,1,Nb), param(Racine_Nb) do
		(for(_,1,Racine_Nb) do	 write('#')
		),
	 	Mod is J mod Racine_Nb, 
	 	(Mod = 0 -> write('#');write('|'))
	 ),
	 nl,
	 	%affichage de chaque ligne
	(for(K,1,Nb), param(Nb,Racine_Nb,Grid) do
		Ligne is Grid[K,1..Nb],
		write('#'),
		(foreach(Var, Ligne),for(Col,1,Nb),param(Racine_Nb) do
				%affichage de chaque variable
			display_var2(Var),
	 		Mod is Col mod Racine_Nb,
	 		(Mod = 0 -> write('#');write('|'))
	 	),
	 	nl,
			%barres intermediaires
		write('#'),
		Mod is K mod Racine_Nb,
		(Mod = 0 -> Char = '#'; Char='-'),
		(for(J,1,Nb), param(Racine_Nb,Char) do
			(for(_,1,Racine_Nb),param(Char) do	 write(Char)
			),
	 		Mod is J mod Racine_Nb, 
	 		(Mod = 0 -> write('#');write('|'))
	 	),
	 	nl
	 ).
	 
	 
display_var1(Var, Nb) :-
	dom(Var,Domain),
	length(Domain,Dom_Size),
	Nb_Blancs is Nb-Dom_Size,
	
	%affichage de chaque variable
	(for(_,1,Nb_Blancs//2) do write(' ')),
  
	(foreach(Val, Domain) do write(Val)),
 
	(for(_,1,(Nb_Blancs+1)//2) do write(' ')).
	
	
	

display_var2(V) :-
	(var(V)
		->	write('   ')
		;
			write(' '), write(V), write(' ')
	).