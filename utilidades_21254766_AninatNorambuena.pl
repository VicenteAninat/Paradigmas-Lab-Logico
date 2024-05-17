:- module(utilidades_21254766_AninatNorambuena,
         [deleteDuplicate/2, cortarListaInferior/3, cortarListaSuperior/3, listAdd/3, verificarAusencia/2, verificarAusenciaListas/2]).
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
verificarAusenciaListas([],[]).
verificarAusenciaListas(List, [H|T]):-
    not(member(H, List)),
    verificarAusenciaListas(List, [T]).
