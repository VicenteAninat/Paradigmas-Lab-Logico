:- module(tdatrain_21254766_AninatNorambuena,
         []).
:- use_module("TDAstation_21254766_AninatNorambuena").
:- use_module("TDAsection_21254766_AninatNorambuena").
:- use_module("TDAline_21254766_AninatNorambuena").
:- use_module("TDApcar_21254766_AninatNorambuena").
%TDA train

% Constructor
% RF10: train
%
% Descripcion:
% Dominio:
% Predicado:
% Metas:
% Submetas:
% Clausulas:
train(Id, Maker, RailType, Speed, Pcars, [Id, Maker, RailType, Speed, Pcars]):-
    number(Id),
    string(Maker),
    string(RailType),
    number(Speed),
    Speed >= 0,
    carrosValidos(Pcars).


% Modificadores
% RF11: trainAddCar
%
% Descripcion:
% Dominio:
% Predicado:
% Metas:
% Submetas:
% Clausulas:
trainAddCar(Train, Pcar, Position, TrainOut):-
    number(Position),
    Position >= 0,
    getTrainPcar(Train, ListaPcar),
    verificarAusencia(ListaPcar, Pcar),
    verificarPosicion(ListaPcar, Position),
    agregarElemListaIndice(ListaPcar, Position, NuevaListaPcar),
    getTrainId(Train, TrainId),
    getTrainMaker(Train, TrainMaker),
    getTrainRailType(Train, TrainRailType),
    getTrainSpeed(Train, TrainSpeed),
    TrainOut = [TrainId, TrainMaker, TrainRailType, TrainSpeed, NuevaListaPcar].

% RF12: trainRemoveCar
%
% Descripcion:
% Dominio:
% Predicado:
% Metas:
% Submetas:
% Clausulas:
trainRemoveCar(Train, Position, TrainOut):-
    number(Position),
    Position >= 0,
    getTrainPcar(Train, ListaPcar),
    verificarPosicion(ListaPcar, Position),
    eliminarElemListaIndice(ListaPcar, Position, NuevaListaPcar),
    getTrainId(Train, TrainId),
    getTrainMaker(Train, TrainMaker),
    getTrainRailType(Train, TrainRailType),
    getTrainSpeed(Train, TrainSpeed),
    TrainOut = [TrainId, TrainMaker, TrainRailType, TrainSpeed, NuevaListaPcar].

% Pertenencia
% RF13: isTrain
%
% Descripcion:
% Dominio:
% Predicado:
% Metas:
% Submetas:
% Clausulas:
isTrain(Train):-
    getTrainId(Train, TrainId),
    getTrainMaker(Train, TrainMaker),
    getTrainRailType(Train, TrainRailType),
    getTrainSpeed(Train, TrainSpeed),
    getTrainPcars(Train, TrainPcars),
    number(TrainId),
    string(TrainMaker),
    string(TrainRailType),
    number(TrainSpeed),
    TrainSpeed >= 0,
    carrosValidos(TrainPcars).

% Auxiliares
% RF14: trainCapacity
%
% Descripcion:
% Dominio:
% Predicado:
% Metas:
% Submetas:
% Clausulas:
trainCapacity(Train, Capacity):-
    getTrainPcars(Train, TrainPcars),
    capacidadTren(TrainPcars, Capacity).
