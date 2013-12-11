:- lib(fd). 

sendmore(L) :-
    csp(L),
    labeling_print(L).

csp(L) :- 
    variables(L), 
    domains(L), 
    constraints(L). 

variables(L) :- 
    L = [S, E, N, D, M, O, R, Y]. 

domains(L) :-
    L :: 0..9. 

constraints(L) :- 
    L = [S, E, N, D, M, O, R, Y],
    S #> 0,
    M #> 0,
    1000 * S + 100 * E + 10 * N + D + 1000 * M + 100 * O + 10 * R + E #= 10000 * M + 1000 * O + 100 * N + 10 * E + Y,
    alldifferent(L).

labeling_print(L) :-
    (foreach(X, L) do dom(X, D), write(D), write(' '), indomain(X), write(' => '), write(X), nl).
