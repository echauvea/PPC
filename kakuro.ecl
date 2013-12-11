:- use_module(display_kakuro).

:- lib(fd).
:- lib(fd_global).

kakuro(N):-
    csp(N, G),
    term_variables(G, L),
    labeling(L),
    disp(G, no).

csp(N, G) :-
    grid_kakuro(N, G),
    domains(G),
    constraints(G).

grid_kakuro(1, G) :- 
    G =[]( 
	       [](vide ,vide ,b(10,4) ,b(6,3) ,vide ,vide ,b(11,4) ,b( 7,3),vide ), 
	       [](vide ,d(4,2) ,_ ,_ ,b(11,4),d( 6,2) ,_ ,_ ,b(3,2)), 
	       [](vide ,[b(3,2),d(8,3)],_ ,_ ,_ ,d( 6,3) ,_ ,_ ,_ ), 
	       [](d(10,4),_ ,_ ,_ ,_ ,[b(10,4),d(7,3)],_ ,_ ,_ ), 
	       [](d( 3,2),_ ,_ ,[b(17,4),d(6,3)],_ ,_ ,_ ,b(15,4),b(4,2)), 
	       [](vide ,b(16,2),[b(23,3),d(6,3)],_ ,_ ,_ ,[b(6,3),d(3,2)],_ ,_ ), 
	       [](d(24,3),_ ,_ ,_ ,d(10,4),_ ,_ ,_ ,_ ), 
	       [](d(23,3),_ ,_ ,_ ,d( 6,3),_ ,_ ,_ ,vide ), 
	       []( vide ,d(8,2) ,_ ,_ ,vide ,d(12,2) ,_ , _ ,vide ) 
	   ).

domains(G) :- 
    dim(G, [NLines, NCols]),
    (multifor([I,J], [1,1], [NLines, NCols]), param(G) do X is G[I, J], (var(X) -> X :: 1..9 ; true)).

constraint(X, _, _, _) :-
    var(X), !.

constraint(b(V,N), I, J, G) :-
    NB is G[I+1 .. I+N, J],
    fd_global:alldifferent(NB),
    sumlist(NB, V).

constraint(d(V,N), I, J, G) :-
    NB is G[I, J+1 .. J+N],
    fd_global:alldifferent(NB),
    sumlist(NB, V).

constraint(vide, _, _, _).

constraint([A, B], I, J, G) :-
    constraint(A, I, J, G),
    constraint(B, I, J, G).

constraints(G) :-
    dim(G, [NLines, NCols]),
    (multifor([I,J], [1,1], [NLines, NCols]), param(G) do X is G[I, J], constraint(X, I, J, G)).

