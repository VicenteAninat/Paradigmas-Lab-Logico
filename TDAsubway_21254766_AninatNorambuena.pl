:- module(tdasubway_21254766_AninatNorambuena,
         []).
:- use_module("utilidades_21254766_AninatNorambuena").
%TDA subway

% Constructor
% RF16: subway
%
% Descripcion:
% Dominio:
% Predicado:
% Metas:
% Submetas:
% Clausulas:
subway(Id, Nombre, [Id, Nombre, [], [], []]):-
    number(Id),
    string(Nombre).

% Selectores
%
% Descripcion:
% Dominio:
% Predicado:
% Metas:
% Submetas:
% Clausulas:
getSubwayId([Id,_,_,_,_], Id).

% Descripcion:
% Dominio:
% Predicado:
% Metas:
% Submetas:
% Clausulas:
getSubwayNombre([_,Nombre,_,_,_], Nombre).

% Descripcion:
% Dominio:
% Predicado:
% Metas:
% Submetas:
% Clausulas:
getSubwayTrain([_,_,Trains,_,_], Trains).

% Descripcion:
% Dominio:
% Predicado:
% Metas:
% Submetas:
% Clausulas:
getSubwayLine([_,_,_,Lines,_], Lines).

% Descripcion:
% Dominio:
% Predicado:
% Metas:
% Submetas:
% Clausulas:
getSubwayDriver([_,_,_,_,Drivers], Drivers).

% Modificadores
% RF17: subwayAddTrain
%
% Descripcion:
% Dominio:
% Predicado:
% Metas:
% Submetas:
% Clausulas:
subwayAddTrain(Sub, Trains, SubwayOut):-
    getSubwayTrain(Sub, SubwayTrains),
    verificarAusenciaListas(SubwayTrains, Trains),
    append(SubwayTrains, Trains, NuevoSubwayTrains),
    getSubwayId(Sub, SubId),
    getSubwayNombre(Sub, SubNombre),
    getSubwayLine(Sub, SubLine),
    getSubwayDriver(Sub, SubDriver),
    SubwayOut = [SubId, SubNombre, NuevoSubwayTrains, SubLine, SubDriver].

% RF18: subwayAddLine
%
% Descripcion:
% Dominio:
% Predicado:
% Metas:
% Submetas:
% Clausulas:
subwayAddLine(Sub, Lines, SubwayOut):-
    getSubwayLine(Sub, SubwayLines),
    verificarAusenciaListas(SubwayLines, Lines),
    append(SubwayLines, Lines, NuevoSubwayLines),
    getSubwayId(Sub, SubId),
    getSubwayNombre(Sub, SubNombre),
    getSubwayTrain(Sub, SubTrain),
    getSubwayDriver(Sub, SubDriver),
    SubwayOut = [SubId, SubNombre, SubTrain, NuevoSubwayLines, SubDriver].

% RF19: subwayAddDriver
%
% Descripcion:
% Dominio:
% Predicado:
% Metas:
% Submetas:
% Clausulas:
subwayAddDriver(Sub, Drivers, SubwayOut):-
    getSubwayDriver(Sub, SubwayDrivers),
    verificarAusenciaListas(SubwayDrivers, Drivers),
    append(SubwayDrivers, Drivers, NuevoSubwayDrivers),
    getSubwayId(Sub, SubId),
    getSubwayNombre(Sub, SubNombre),
    getSubwayTrain(Sub, SubTrain),
    getSubwayLine(Sub, SubLine),
    SubwayOut = [SubId, SubNombre, SubTrain, SubLine, NuevoSubwayDrivers].

% RF20: subwayToString
%
% Descripcion:
% Dominio:
% Predicado:
% Metas:
% Submetas:
% Clausulas:
subwayToString(Sub, StrOut):-
    atomics_to_string(Sub, StrOut).
