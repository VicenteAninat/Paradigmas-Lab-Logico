:- module(tdasubway_21254766_AninatNorambuena,
         [subway/3, subwayAddDriver/3, subwayAddLine/3, subwayAddTrain/3]).
:- use_module("utilidades_21254766_AninatNorambuena").
% TDA subway

% Constructor
/*
RF16: subway

Descripcion: Predicado que permite crear una red de metro.
Dominio: Id(int), Nombre(string)
Predicado: subway(Id, Nombre, [Id, Nombre, [], [], []])
Metas: subway/1
Submetas: number/1, string/1
Clausulas:
*/
subway(Id, Nombre, [Id, Nombre, [], [], []]):-
    number(Id),
    string(Nombre).

% Selectores
/*
Descripcion: Predicado que extrae el Id de una red de metro
Dominio: Id(int)
Predicado: getSubwayId([Id,_,_,_,_], Id)
Metas: getSubwayId/2
Submetas:
Clausulas:
*/
getSubwayId([Id,_,_,_,_], Id).

/*
Descripcion: Predicado que extrae el nombre de una red de metro
Dominio: Nombre(string)
Predicado: getSubwayNombre([_,Nombre,_,_,_], Nombre)
Metas: getSubwayNombre/2
Submetas:
Clausulas:
*/
getSubwayNombre([_,Nombre,_,_,_], Nombre).

/*
Descripcion: Predicado que extrae los trenes de una red de metro
Dominio: Trains(train list)
Predicado: getSubwayTrain([_,_,Trains,_,_], Trains)
Metas: getSubwayTrain/2
Submetas:
Clausulas:
*/
getSubwayTrain([_,_,Trains,_,_], Trains).

/*
Descripcion: Predicado que extrae las lineas de una red de metro
Dominio: Lines(line list)
Predicado: getSubwayLine([_,_,_,Lines,_], Lines)
Metas: getSubwayLine/2
Submetas:
Clausulas:
*/
getSubwayLine([_,_,_,Lines,_], Lines).

/*
Descripcion: Predicado que extrae los conductores de una red de metro
Dominio: Drivers(driver list)
Predicado: getSubwayDriver([_,_,_,_,Drivers], Drivers)
Metas: getSubwayDriver/2
Submetas:
Clausulas:
*/
getSubwayDriver([_,_,_,_,Drivers], Drivers).


% Modificadores
/*
RF17: subwayAddTrain

Descripcion: Predicado que permite añadir trenes a una red de metro.
Dominio: Sub(subway), Trains(train list), SubwayOut(subway)
Predicado: subwayAddTrain(Sub, Trains, SubwayOut)
Metas: subwayAddTrain/3
Submetas: getSubwayTrain/2, verificarAusenciaListas/2, append/3,
getSubwayId/2, getSubwayNombre/2, getSubwayLine/2, getSubwayDriver/2
Clausulas:
*/
subwayAddTrain(Sub, Trains, SubwayOut):-
    getSubwayTrain(Sub, SubwayTrains),
    verificarAusenciaListas(SubwayTrains, Trains),
    append(SubwayTrains, Trains, NuevoSubwayTrains),
    getSubwayId(Sub, SubId),
    getSubwayNombre(Sub, SubNombre),
    getSubwayLine(Sub, SubLine),
    getSubwayDriver(Sub, SubDriver),
    SubwayOut = [SubId, SubNombre, NuevoSubwayTrains, SubLine, SubDriver].

/*
RF18: subwayAddLine

Descripcion: Predicado que permite añadir líneas a una red de metro.
Dominio: Sub(subway), Lines(line list), SubwayOut(subway)
Predicado: subwayAddLine(Sub, Lines, SubwayOut)
Metas: subwayAddLine/3
Submetas: getSubwayLine/2, verificarAusenciaListas/2, append/2,
getSubwayId/2, getSubwayNombre/2, getSubwayTrain/2, getSubwayDriver/2
Clausulas:
*/
subwayAddLine(Sub, Lines, SubwayOut):-
    getSubwayLine(Sub, SubwayLines),
    verificarAusenciaListas(SubwayLines, Lines),
    append(SubwayLines, Lines, NuevoSubwayLines),
    getSubwayId(Sub, SubId),
    getSubwayNombre(Sub, SubNombre),
    getSubwayTrain(Sub, SubTrain),
    getSubwayDriver(Sub, SubDriver),
    SubwayOut = [SubId, SubNombre, SubTrain, NuevoSubwayLines, SubDriver].

/*
RF19: subwayAddDriver

Descripcion: Predicado que permite añadir conductores a una red de metro.
Dominio: Sub(subway), Drivers(driver list), SubwayOut(subway)
Predicado: subwayAddDriver(Sub, Drivers, SubwayOut)
Metas: subwayAddDriver/3
Submetas: getSubwayDriver/2, verificarAusenciaListas/2, append/3,
getSubwayId/2, getSubwayNombre/2, getSubwayTrain/2, getSubwayLine/2
Clausulas:
*/
subwayAddDriver(Sub, Drivers, SubwayOut):-
    getSubwayDriver(Sub, SubwayDrivers),
    verificarAusenciaListas(SubwayDrivers, Drivers),
    append(SubwayDrivers, Drivers, NuevoSubwayDrivers),
    getSubwayId(Sub, SubId),
    getSubwayNombre(Sub, SubNombre),
    getSubwayTrain(Sub, SubTrain),
    getSubwayLine(Sub, SubLine),
    SubwayOut = [SubId, SubNombre, SubTrain, SubLine, NuevoSubwayDrivers].

/*
RF20: subwayToString

Descripcion: Función que permite expresar una red de metro en un formato String.
Dominio: Sub(subway), StrOut(string)
Predicado: subwayToString(Sub, StrOut)
Metas: subwayToString/2
Submetas:
Clausulas:
*/
subwayToString(Sub, StrOut):-
    atomics_to_string(Sub, StrOut).
