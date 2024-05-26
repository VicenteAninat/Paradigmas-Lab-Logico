:- module(tdapcar_21254766_AninatNorambuena,
         [pcar/5, carrosValidos/1, getPcarCapacity/2]).
:- use_module(utilidades_21254766_aninatnorambuena).
% TDA pcar

/*
Descripcion: Predicado que define el tipo de dato carType como tipo de
carro
Dominio: X(atomo)
Predicado: carType(X)
Metas: carType/1
Submetas:
Clausulas:
*/
carType(X):-
    X = ct;
    X = tr.

% Constructor
/*
RF9: pcar

Descripcion: Permite crear los carros de pasajeros que conforman un
convoy. Los carros pueden ser de tipo terminal (tr) o central (ct)
Dominio: Id(int), Capacity(int), Model(string), Type(carType)
Predicado: pcar(Id, Capacity, Model, Type, [Id, Capacity, Model, Type])
Metas: pcar/1
Submetas: number/1, string/1, carType/1
Clausulas:
*/
pcar(Id, Capacity, Model, Type, [Id, Capacity, Model, Type]):-
    number(Id),
    number(Capacity),
    Capacity >= 0,
    string(Model),
    carType(Type).

/*
Descripcion: Predicado que extrae el tipo de carro de un carro
Dominio: Type(carType)
Predicado: getPcarType([_,_,_,Type], Type)
Metas: getPcarType/2
Submetas:
Clausulas:
*/
getPcarType([_,_,_,Type], Type).

/*
Descripcion: Predicado que extrae la capacidad de un carro
Dominio: Capacity
Predicado: getPcarCapacity([_,Capacity,_,_], Capacity)
Metas: getPcarCapacity/2
Submetas:
Clausulas:
*/
getPcarCapacity([_,Capacity,_,_], Capacity).

/*
Descripcion: Predicado que comprueba que el primer y el ultimo carro de
una lista sean de tipo terminal
Dominio: ListaCarros(pcar list)
Predicado: extremosValidos(ListaCarros)
Metas: extremosValidos/1
Submetas: last/2, reverse/2, getPcarType/2
Clausulas:
*/
extremosValidos(ListaCarros):-
    last(ListaCarros, UltimoCarro),
    reverse(ListaCarros, ListaCarrosInv),
    last(ListaCarrosInv, PrimerCarro),
    getPcarType(UltimoCarro, Type1),
    getPcarType(PrimerCarro, Type2),
    Type1 = Type2,
    Type1 = tr.

/*
Descripcion: Predicado que comprueba que todos los carros de una lista
sean de tipo central
Dominio: H(cabeza de lista), T(cola de lista)
Predicado: centroValido([])
centroValido([H|T])
Metas: centroValido/1
Submetas: getPcarType/2, centroValido/1(recursividad)
Clausulas:
*/
centroValido([]).
centroValido([H|T]):-
    getPcarType(H, Type),
    Type = ct,
    centroValido(T).

/*
Descripcion: Predicado que comprueba que todos los carros de una lista
sean validos para un tren, teniendo extremos tipo tr y centro
tipo ct
Dominio: ListaCarros(pcar list)
Predicado: carrosValidos(ListaCarros)
Metas: carrosValidos/1
Submetas: length/2, extremosValidos/1,
Clausulas:
*/
carrosValidos(ListaCarros):-
    length(ListaCarros, Largo),
    Largo \= 0,
    Largo \= 1,
    ((Largo = 2, extremosValidos(ListaCarros));
    extremosValidos(ListaCarros)),
    cortarExtremos(ListaCarros, NuevaListaCarros),
    centroValido(NuevaListaCarros).
