:- lib(fd). 

% Donne le problème et affiche la 1ère solution trouvée (+ more pour afficher les suivantes). 
main1(X) :- 
    csp(X), 
    instancier(X). 

% Donne le problème et donne l'ensemble des solutions.
main2(S) :- 
    csp(X), 
    findall(X, instancier(X), S). 

% Donne le problème, l'ensemble des solutions, et affiche le domaine de chaque variable à chaque étape de la résolution.
main3(S) :-
    csp(X),
    findall(X, instancier2(X), S).

% Prédicat qui pose le problème sans le rédoure :
%   - On définit les variables du CSP [A, B, C]
%   - Leurs domaines (le même pour tous)
%   - Les contraintes du CSP
csp(X) :- 
    variables(X), 
    domaines(X), 
    contraintes(X). 

% Stocke dans X une liste de 3 variables non instanciées.
variables(X) :- 
    X = [A,B,C]. 

% Définit le domaine de la variable (ou des variables de la liste) X.
domaines(X) :- 
    X :: [bleu, rouge]. 

% Définit les contraintes sur les variables de X.
contraintes(X) :- 
    X = [A,B,C], 
    A #\= B, 
    B #\= C. %, 
%    A #\= C. 

% Equivalent à labeling
% Instancie chaque n-uplet de variables possibles parmis X
instancier(Vars) :- 
    ( foreach(X, Vars) do indomain(X) ).

% Instancie chaque n-uplet de variables possibles parmis X
instancier2(Vars) :- 
    ( foreach(X, Vars) do dom(X, L), write(L), write(' '), indomain(X) ),
    nl.
