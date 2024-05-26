:- module(utilidades_21254766_AninatNorambuena,
         [deleteDuplicate/2, cortarListaInferior/3, cortarListaSuperior/3, listAdd/3, verificarAusencia/2, verificarAusenciaListas/2, cortarExtremos/2, verificarPosicion/2, agregarElemListaIndice/4, eliminarElemListaIndice/3]).
% Utilidades

/*
Descripcion: Predicado que filtra un elemento de una lista
Dominio: X(elemento de lista), Y(Elemento de lista), Elem(elemento de
lista), Lista(list), R(list)
Predicado: objetivo(X, Y)
filtrar(Elem, Lista, R)
Metas: objetivo/2, filtrar/3
Submetas: exclude/3
Clausulas:
*/
objetivo(X, Y):- X = Y.
filtrar(Elem, Lista, R):- exclude(objetivo(Elem), Lista, R).

/*
Descripcion: Predicado que devuelve una lista sin elementos repetidos
Dominio: H(Cabeza de lista), L(cola de lista), R(list)
Predicado: deleteDuplicate([],[])
deleteDuplicate([H|L], R)
Metas: deleteDuplicate/2
Submetas: filtrar/3, deleteDuplicate/2(recursividad)
Clausulas:
*/
deleteDuplicate([],[]). %Caso base
deleteDuplicate([H|L], R):- filtrar(H, L, LFiltrada), R=[H|Y], deleteDuplicate(LFiltrada, Y).

/*
Descripcion: Predicado que corta una lista a partir de un elemento
Dominio: H(cabeza de lista), T(cola de lista), Elem(Elemento de lista),
NuevaLista(list)
Predicado: cortarListaInferior([], _, [])
cortarListaInferior([H|T], Elem, NuevaLista)
cortarListaInferior([H|T], Elem, NuevaLista)
Metas: cortarListaInferior/3
Submetas: cortarListaInferior/3(recursividad)
Clausulas:
*/
cortarListaInferior([], _, []).
cortarListaInferior([H|T], Elem, NuevaLista):-
    H \= Elem,
    cortarListaInferior(T, Elem, NuevaLista).
cortarListaInferior([H|T], Elem, NuevaLista):-
    H = Elem,
    NuevaLista = [H|T].

/*
Descripcion: Predicado que corta una lista hasta un elemento
Dominio: H(cabeza de lista), T(cola de lista), Elem(Elemento de lista),
NuevaLista(list)
Predicado: cortarListaSuperior([], _, [])
cortarListaSuperior([H|T], Elem, NuevaLista)
cortarListaSuperior([H|_], Elem, NuevaLista)
Metas: cortarListaSuperior/3
Submetas: cortarListaSuperior/3(recursividad)
Clausulas:
*/
cortarListaSuperior([], _, []).
cortarListaSuperior([H|T], Elem, NuevaLista):-
    H \= Elem,
    cortarListaSuperior(T, Elem, NuevaLista2),
    NuevaLista = [H|NuevaLista2].
cortarListaSuperior([H|_], Elem, NuevaLista):-
    H = Elem,
    NuevaLista = [H].

/*
Descripcion: Predicado que agrega un elemento a una lista
Dominio: H(cabeza de lista), T(cola de lista), Elem(elemento de lista),
NuevaLista(list)
Predicado: listAdd([],Elem, [Elem])
listAdd([H|T], Elem, NuevaLista)
Metas: listAdd/3
Submetas: listAdd/3(recursividad)
Clausulas:
*/
listAdd([],Elem, [Elem]).
listAdd([H|T], Elem, NuevaLista):-
    listAdd(T, Elem, NuevaLista2),
    NuevaLista = [H, NuevaLista2].

/*
Descripcion: Predicado que comprueba la ausencia de un elemento en una
lista
Dominio: H(cabeza de lista), T(cola de lista), Elem(elemento de lista)
Predicado: verificarAusencia([],_)
verificarAusencia([H|T], Elem)
Metas: verificarAusencia/2
Submetas: verificarAusencia/2(recursividad)
Clausulas:
*/
verificarAusencia([],_).
verificarAusencia([H|T], Elem):-
    H \= Elem,
    verificarAusencia(T, Elem).

/*
Descripcion: Predicado que comprueba que ningun elemento de una lista se
repita en otra
Dominio: List(list), H(cabeza de lista), T(cola de lista)
Predicado: verificarAusenciaListas(_,[])
verificarAusenciaListas([],_)
verificarAusenciaListas(List, [H|T])
Metas: verificarAusenciaListas/2
Submetas: member/2, verificarAusenciaListas/2(recursividad)
Clausulas:
*/
verificarAusenciaListas(_,[]).
verificarAusenciaListas([],_).
verificarAusenciaListas(List, [H|T]):-
    \+ member(H, List),
    verificarAusenciaListas(List, T).

/*
Descripcion: Predicado que es verdadero si la lista resultante es la
primera lista sin el elemento final ni el inicial
Dominio: H(cabeza de lista), T(cola de lista), ListaCortada(list)
Predicado: cortarExtremos([H|T], ListaCortada)
Metas: cortarExtremos/2
Submetas: last/2, select/3, reverse/2
Clausulas:
*/
cortarExtremos([H|T], ListaCortada):-
    Primero = H,
    last([H|T], Ultimo),
    select(Primero, [H|T], Lista1),
    reverse(Lista1, ListaInv),
    select(Ultimo, ListaInv, ListaFinalInv),
    reverse(ListaFinalInv, ListaCortada).

/*
Descripcion: Predicado que es verdadero si una posicion es accesible
dentro de una lista
Dominio: Lista(list), Posicion(int)
Predicado: verificarPosicion(Lista, Posicion)
Metas: verificarPosicion/2
Submetas: length/2
Clausulas:
*/
verificarPosicion(Lista, Posicion):-
    length(Lista, X),
    Posicion =< X.

/*
Descripcion: Predicado que es verdadero si la lista final es la primera
lista con un nuevo elemento agregado en una posicion especifica
Dominio: Lista(list), Elem(elemento de lista), Indice(int),
NuevaLista(list)
Predicado: agregarElemListaIndice(Lista, Elem, Indice, NuevaLista)
agregarElemListaIndice([H|T], Elem, Indice, NuevaLista)
Metas: agregarElemListaIndice/4
Submetas: append/3, agregarElemListaIndice/4(recursividad)
Clausulas:
*/
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

/*
Descripcion: Predicado que es verdadero si la lista final es la primera
lista sin el elemento de una posicion especifica
Dominio: Lista(list), Indice(int), NuevaLista(list)
Predicado: eliminarElemListaIndice(Lista, Indice, NuevaLista)
Metas: eliminarElemListaIndice/3
Submetas: nth0/4
Clausulas:
*/
eliminarElemListaIndice(Lista, Indice, NuevaLista):-
    nth0(Indice, Lista, _, NuevaLista).
