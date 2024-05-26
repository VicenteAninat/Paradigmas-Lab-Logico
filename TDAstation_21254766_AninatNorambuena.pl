:- module(tdastation_21254766_AninatNorambuena,
         [station/5, station/1, encontrarEstacion/3, getStationId/2, getStationName/2, getStationType/2]).
%TDA station
/*
Descripcion: Predicado que define un tipo de estacion de metro de tipo
 c (combinacion), m (mantencion), r (regular) o t (terminal)
Dominio: X(atomo)
Predicado: type(X)
Metas: type/1
Submetas:
Clausulas:
*/
type(X):-
    X == c;
    X == m;
    X == r;
    X == t.

% Constructor
/*
RF2: station

Descripcion: Predicado constructor de una estacion de metro
Dominio: Id(int), Name(string), Type(type), StopTime(int)>=0
Predicado: station(Id, Name, Type, StopTime, [Id, Name, Type, StopTime])
Metas: station/5
Submetas: number/1, string/1, type/1
Clausulas:
*/
station(Id, Name, Type, StopTime, [Id, Name, Type, StopTime]):-
    number(Id),
    string(Name),
    type(Type),
    number(StopTime),
    StopTime >= 0.

% Selectores
/*
Descripcion: Predicado que extrae el ID de una estacion
Dominio: Id(int)
Predicado: getStationId([Id,_,_,_], Id)
Metas: getStationId/2
Submetas:
Clausulas:
*/
getStationId([Id,_,_,_], Id).

/*
Descripcion: Predicado que extrae el ID de una estacion
Dominio: Name(string)
Predicado: getStationName([_,Name,_,_], Name)
Metas: getStationName/2
Submetas:
Clausulas:
*/
getStationName([_,Name,_,_], Name).

/*
Descripcion: Predicado que extrae el ID de una estacion
Dominio: Type(type)
Predicado: getStationType([_,_,Type,_], Type)
Metas: getStationType/2
Submetas:
Clausulas:
*/
getStationType([_,_,Type,_], Type).

% Pertenencia
/*
Descripcion: Predicado que comprueba la validez de una estacion
Dominio: Id(int), Name(string), Type(type), StopTime(int)>=0
Predicado: station([Id, Name, Type, StopTime])
Metas: station/1
Submetas: number/1, string/1, type/1
Clausulas:
*/
station([Id, Name, Type, StopTime]):-
    number(Id),
    string(Name),
    type(Type),
    number(StopTime),
    StopTime >= 0.

% Predicados auxiliares
/*
Descripcion: Encuentra una estacion de una lista en base a su nombre
Dominio: H(cabeza de lista), T(cola de lista), Nombre(string),
Estacion(station)
Predicado: encontrarEstacion([H|_], Nombre, Estacion)
encontrarEstacion([H|T], Nombre, Estacion)
Metas: encontrarEstacion/3
Submetas: getStationName/2, encontrarEstacion/3(recursividad)
Clausulas:
*/
encontrarEstacion([H|T], Nombre, Estacion):-
    getStationName(H, NombreEstacion),
    Nombre \= NombreEstacion,
    encontrarEstacion(T, Nombre, Estacion).
encontrarEstacion([H|_], Nombre, Estacion):-
    getStationName(H, NombreEstacion),
    Nombre = NombreEstacion,
    H = Estacion.
