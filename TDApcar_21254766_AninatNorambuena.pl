:- module(tdapcar_21254766_AninatNorambuena,
         [pcar/5, carrosValidos/1, getPcarCapacity/2]).
:- use_module(utilidades_21254766_aninatnorambuena).
%TDA pcar

carType(X):-
    X = ct;
    X = tr.

% Constructor
% RF9: pcar
%
% Descripcion:
% Dominio:
% Predicado:
% Metas:
% Submetas:
% Clausulas:
pcar(Id, Capacity, Model, Type, [Id, Capacity, Model, Type]):-
    number(Id),
    number(Capacity),
    Capacity >= 0,
    string(Model),
    carType(Type).

% Descripcion:
% Dominio:
% Predicado:
% Metas:
% Submetas:
% Clausulas:
getPcarType([_,_,_,Type], Type).

% Descripcion:
% Dominio:
% Predicado:
% Metas:
% Submetas:
% Clausulas:
getPcarCapacity([_,Capacity,_,_], Capacity).

% Descripcion:
% Dominio:
% Predicado:
% Metas:
% Submetas:
% Clausulas:
extremosValidos(ListaCarros):-
    last(ListaCarros, UltimoCarro),
    reverse(ListaCarros, ListaCarrosInv),
    last(ListaCarrosInv, PrimerCarro),
    getPcarType(UltimoCarro, Type1),
    getPcarType(PrimerCarro, Type2),
    Type1 = Type2,
    Type1 = tr.

% Descripcion:
% Dominio:
% Predicado:
% Metas:
% Submetas:
% Clausulas:
centroValido([]).
centroValido([H|T]):-
    getPcarType(H, Type),
    Type = ct,
    centroValido(T).

% Descripcion:
% Dominio:
% Predicado:
% Metas:
% Submetas:
% Clausulas:
carrosValidos(ListaCarros):-
    length(ListaCarros, Largo),
    Largo \= 0,
    Largo \= 1,
    ((Largo = 2, extremosValidos(ListaCarros));
    extremosValidos(ListaCarros)),
    cortarExtremos(ListaCarros, NuevaListaCarros),
    centroValido(NuevaListaCarros).
