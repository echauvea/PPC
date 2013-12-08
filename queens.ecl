:- lib(fd). 
:- lib(fd_search).

queens(N, X) :-
    csp(X, N),
    labeling(X).

queens_ff(N, X) :- 
    csp(X, N), 
    labeling_ff(X). 
    
queens_ffc(N, X) :- 
    csp(X, N), 
    labeling_ffc(X). 

queens_med(N, X) :-
    csp(X, N),
    labeling_med(X).

queens_ff_med(N, X) :-
    csp(X, N),
    labeling_ff_med(X).

% Prédicat qui pose le problème des N reines sans le rédoure :
%   - On définit les variables du CSP
%   - Leurs domaines
%   - Les contraintes du CSP
csp(X, N) :- 
    variables(X, N), 
    domains(X), 
    constraints(X). 

% Stocke dans L une liste des variables non instanciées.
variables(L, N) :- 
    length(L, N). 

% Définit le domaine de chaque variable.
domains(X) :-
    length(X, N),
    X :: 1..N. 

% Définit les contraintes sur les variables de X.
constraints(X) :- 
    alldifferent(X),
    (foreach(V, X), foreach(VS, DS), foreach(VD, DD), count(I, 0, _) do VS = V + I, VD = V - I),
    alldifferent(DS),
    alldifferent(DD).

% Instancie les variables en utilisant le prédicat deleteff
labeling_ff(L) :- 
    (fromto(L, I, O, []) do deleteff(X, I, O), indomain(X)).
    
% Instancie les variables en utilisant le prédicat deleteffc
labeling_ffc(L) :- 
    (fromto(L, I, O, []) do deleteffc(X, I, O), indomain(X)).

labeling_med(L) :- 
    foreach(X, L) do fd_search:indomain(X, median).

labeling_ff_med(L) :- 
    (fromto(L, I, O, []) do deleteff(X, I, O), fd_search:indomain(X, median)).
