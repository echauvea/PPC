:- module(display_kakuro).
:- export(disp/2).

:- lib(fd).

   % Affichage d'une grille de Kakuro
   % si Verbose = yes, le domaine courant des variables est affiché
   % sinon la case est vide (que des espaces)

disp(G,Verbose) :-
	dim(G,[Nbl, Nbc]),
	(for(Ligne,1, Nbl), param(G, Nbc, Verbose) do
		(for(_C,1,Nbc) do
			write('|---------')
		), write('|'), nl,
		(for(Colonne,1,Nbc), param(G,Ligne, Verbose)do
			E is G[Ligne,Colonne],
			disp_elt(E,Verbose)
		), write('|'),
		nl
	),
	(for(_C,1,Nbc) do
		write('|---------')
	),
	write('|'), nl.

disp_elt(Var,Verbose) :- var(Var), !,
   (Verbose = yes ->
      write('|'), display_dom(Var,9)
      ;
      write('|         ')
   ).
disp_elt(N,_) :- integer(N), write('|   '), wr(N), write('    ').
disp_elt(vide,_) :-
	write('|#########').
disp_elt(b(S,_N),_) :-
	write('| '), wl(S),write(' \\    ').
disp_elt(d(S,_N),_) :-
	write('|    \\ '), wr(S), write(' ').
disp_elt([b(S1,_N1), d(S2,_N2)],_) :-
	write('| '), wl(S1), write(' \\ '), wr(S2), write(' ').

   % Affichage justifie d'un nombre decimal de 2 digits au plus
   % (au Kakuro, une somme ne depasse jamais 45 (=1+2+3+4+5+6+7+8+9))

         % justification a gauche
wl(N) :-
   (N>9 ->
      write(N)
      ;
      write(N),write(' ')
   ).
   
         % justification a droite
wr(N) :-
   (N>9 ->
      write(N)
      ;
      write(' '),write(N)
   ).

   % Affichage centre du domaine d'une variable (entre 1 et 9)

display_dom(Var, Nb) :-
	dom(Var,Domain),
	length(Domain,Dom_Size),
	Nb_Blancs is Nb-Dom_Size,
	
	%affichage de chaque variable
	(for(_,1,Nb_Blancs//2) do write(' ')),
  
	(foreach(Val, Domain) do write(Val)),
 
	(for(_,1,(Nb_Blancs+1)//2) do write(' '))