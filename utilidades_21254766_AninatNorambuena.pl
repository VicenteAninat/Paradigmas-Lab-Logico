:- module(utilidades_21254766_AninatNorambuena,
         [deleteDuplicate/2, cortarListaInferior/3, cortarListaSuperior/3, listAdd/3, verificarAusencia/2, verificarAusenciaListas/2, cortarExtremos/2, verificarPosicion/2, agregarElemListaIndice/4, eliminarElemListaIndice/3]).
% Utilidades

% Descripcion:
% Dominio:
% Predicado:
% Metas:
% Submetas:
% Clausulas:
objetivo(X, Y):- X = Y.
filtrar(Elem, Lista, R):- exclude(objetivo(Elem), Lista, R).

% Descripcion:
% Dominio:
% Predicado:
% Metas:
% Submetas:
% Clausulas:
deleteDuplicate([],[]). %Caso base
deleteDuplicate([H|L], R):- filtrar(H, L, LFiltrada), R=[H|Y], deleteDuplicate(LFiltrada, Y).

% Descripcion:
% Dominio:
% Predicado:
% Metas:
% Submetas:
% Clausulas:
cortarListaInferior([], _, []).
cortarListaInferior([H|T], Elem, NuevaLista):-
    H \= Elem,
    cortarListaInferior(T, Elem, NuevaLista).
cortarListaInferior([H|T], Elem, NuevaLista):-
    H = Elem,
    NuevaLista = [H|T].

% Descripcion:
% Dominio:
% Predicado:
% Metas:
% Submetas:
% Clausulas:
cortarListaSuperior([], _, []).
cortarListaSuperior([H|T], Elem, NuevaLista):-
    H \= Elem,
    cortarListaSuperior(T, Elem, NuevaLista2),
    NuevaLista = [H|NuevaLista2].
cortarListaSuperior([H|_], Elem, NuevaLista):-
    H = Elem,
    NuevaLista = [H].

% Descripcion:
% Dominio:
% Predicado:
% Metas:
% Submetas:
% Clausulas:
listAdd([],Elem, [Elem]).
listAdd([H|T], Elem, NuevaLista):-
    listAdd(T, Elem, NuevaLista2),
    NuevaLista = [H, NuevaLista2].

% Descripcion:
% Dominio:
% Predicado:
% Metas:
% Submetas:
% Clausulas:
verificarAusencia([],_).
verificarAusencia([H|T], Elem):-
    H \= Elem,
    verificarAusencia(T, Elem).

% Descripcion:
% Dominio:
% Predicado:
% Metas:
% Submetas:
% Clausulas:
verificarAusenciaListas(_,[]).
verificarAusenciaListas([],_).
verificarAusenciaListas(List, [H|T]):-
    \+ member(H, List),
    verificarAusenciaListas(List, T).

% Descripcion:
% Dominio:
% Predicado:
% Metas:
% Submetas:
% Clausulas:
cortarExtremos([H|T], ListaCortada):-
    Primero = H,
    last([H|T], Ultimo),
    select(Primero, [H|T], Lista1),
    reverse(Lista1, ListaInv),
    select(Ultimo, ListaInv, ListaFinalInv),
    reverse(ListaFinalInv, ListaCortada).

% Descripcion:
% Dominio:
% Predicado:
% Metas:
% Submetas:
% Clausulas:
verificarPosicion(Lista, Posicion):-
    length(Lista, X),
    Posicion =< X.

% Descripcion:
% Dominio:
% Predicado:
% Metas:
% Submetas:
% Clausulas:
agregarElemListaIndice(Lista, Elem, Indice, NuevaLista):-
    Indice = 0,
    ListaElem = [Elem],
    append(ListaElem, Lista, NuevaLista).
agregarElemListaIndice([H|T], Elem, Indice, NuevaLista):-
    Indice \= 0,
    Uno = 1,
    Indice2 is Indice - Uno,
    agregarElemListaIndice(T, Elem, Indice2, Lista2),
    append([H], Lista2, NuevaLista).

% Descripcion:
% Dominio:
% Predicado:
% Metas:
% Submetas:
% Clausulas:
eliminarElemListaIndice(Lista, Indice, NuevaLista):-
    nth0(Indice, Lista, _, NuevaLista).
