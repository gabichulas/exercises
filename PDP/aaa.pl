primero([X|_], X).

resto([_|Xs], Xs).

construye(X, L1, [X|L1]).

pertenece(_, []).
pertenece(X, [X|_]).
pertenece(X, [_|Xs]):- pertenece(X,Xs).

concatena([], X, X).
concatena([X|Xs], Y, [X|Zs]):- concatena(Xs, Y, Zs).

inversa([], []).
inversa([X|Xs], L1):- inversa(Xs, L2), concatena(L2, [X], L1).

palindromo(L):- inversa(L, L).

ultimo(X, L):- inversa(L, [X|_]).


select(X, [X|Xs], Xs).
select(X, [Y|Ys], [Y|Resto]):- select(X, Ys, Resto). 

inserta(X, L1, L2):- select(X, L2, L1).

sublista(L1, L2):- append(_, Resto, L2), append(L1, _, Resto).

permutacion([], []).
permutacion([X|Xs], L2):- select(X, L2, Resto), permutacion(Xs, Resto).

subconjunto(_, []).
subconjunto(L1, [X|Xs]):- select(X, L1, Resto), subconjunto(Resto, Xs).

maximo(X, Y, X):- X >= Y.
maximo(X, Y, Y):- Y > X.

multirot(_, []).
multirot(L1, L2):- length(L1, N), length(Grupo, N), append(Grupo, Resto, L2), permutacion(L1, Grupo), multirot(L1, Resto).

construirLista(0, []).
construirLista(N, L):- N > 0, construirLista_aux(N, N, L).

construirLista_aux(_, 0, []).
construirLista_aux(Valor, Contador, [Valor|Xs]):- 
    Contador > 0, 
    Contador1 is Contador - 1, 
    construirLista_aux(Valor, Contador1, Xs).

telescopio(0, []).
telescopio(N, L1):- N > 0, N1 is N-1, construirLista(N, A), concatena(A, Resto, L1), telescopio(N1, Resto).

consecutivas(N, L1):- telescopio(N, L1)