:- module(tdapcar_21254766_AninatNorambuena,
         []).
%TDA pcar

carType(X):-
    X == ct,
    X == tr.

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
