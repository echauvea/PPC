:- use_module(display_sudoku).

:- lib(fd).
:- lib(fd_global).

sudoku(Num):-
    csp(Num, G),
    term_variables(G, L),
    labeling(L),
    display_grid(G, no).

csp(Num, G) :-
    grid(Num, NF, G),
    domains(G),
    constraints(G).

grid(6,9,T) :- 
    T = []( 
		[](1,7,_,_,3,_,_,_,_), 
		[](_,_,_,_,1,_,_,_,8), 
		[](6,_,_,_,8,5,_,_,_), 
		[](_,_,_,_,_,_,9,_,2), 
		[](_,_,7,_,_,_,_,6,_), 
		[](_,2,_,_,9,4,_,_,1), 
		[](_,_,2,_,_,_,_,9,_), 
		[](_,8,_,1,2,_,_,_,_), 
		[](7,_,6,_,_,_,_,_,_) 
	    ). 

grid(24,9,T) :- 
    T = []( 
		[](_,1,_,_,_,3,9,_,_), 
		[](_,_,_,7,_,_,5,_,_), 
		[](_,4,_,_,_,_,_,_,8), 
		[](4,_,_,_,_,_,1,_,_), 
		[](_,_,8,5,_,_,_,_,_), 
		[](_,_,_,_,1,_,_,2,3), 
		[](_,_,5,6,8,_,_,_,_), 
		[](_,_,_,_,2,_,_,4,_), 
		[](_,7,_,_,_,_,_,_,6) 
	    ).
domains(G) :- 
    dim(G, [NLines, NCols]),
    (multifor([L,C], [1,1], [NLines, NCols]), param(G), param(NLines) do X is G[L, C], (var(X) -> X :: 1..NLines ; true)).

constraints(G) :-
    dim(G, [NL, NL]),
    N*N is NL,
    (for(L, 1, NL), param(G), param(NL) do Line is G[L], Column is G[1..NL, L], fd_global:alldifferent(Line), fd_global:alldifferent(Column)),
    (multifor([I,J], [1,1], [N, N]), param(G) 
    do 
      Block is G[(I-1)*N+1 .. I*N, (J-1)*N+1 .. J*N],
      write(Block),
      fd_global:alldifferent(Block)
    ).

