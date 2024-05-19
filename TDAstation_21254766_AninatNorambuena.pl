:- module(tdastation_21254766_AninatNorambuena,
         [station/1, encontrarEstacion/3, getStationId/2, getStationName/2, getStationType/2]).
%TDA station

% Descripcion:
% Dominio:
% Predicado:
% Metas:
% Submetas:
% Clausulas:
type(X):-
    X = "c";
    X = "m";
    X = "r";
    X = "t".

% Constructor
% RF2: station
%
% Descripcion:
% Dominio:
% Predicado:
% Metas:
% Submetas:
% Clausulas:
station(Id, Name, Type, StopTime, [Id, Name, Type, StopTime]):-
    number(Id),
    string(Name),
    type(Type),
    number(StopTime),
    StopTime >= 0.

% Selectores
%
% Descripcion:
% Dominio:
% Predicado:
% Metas:
% Submetas:
% Clausulas:
getStationId([Id,_,_,_], Id).

% Descripcion:
% Dominio:
% Predicado:
% Metas:
% Submetas:
% Clausulas:
getStationName([_,Name,_,_], Name).

% Descripcion:
% Dominio:
% Predicado:
% Metas:
% Submetas:
% Clausulas:
getStationType([_,_,Type,_], Type).

% Pertenencia
%
% Descripcion:
% Dominio:
% Predicado:
% Metas:
% Submetas:
% Clausulas:
station([Id, Name, Type, StopTime]):-
    number(Id),
    string(Name),
    type(Type),
    number(StopTime).

% Predicados auxiliares
% Descripcion:
% Dominio:
% Predicado:
% Metas:
% Submetas:
% Clausulas:
encontrarEstacion([H|T], Nombre, Estacion):-
    getStationName(H, NombreEstacion),
    Nombre \= NombreEstacion,
    encontrarEstacion(T, Nombre, Estacion).
encontrarEstacion([H|_], Nombre, Estacion):-
    getStationName(H, NombreEstacion),
    Nombre = NombreEstacion,
    H = Estacion.
