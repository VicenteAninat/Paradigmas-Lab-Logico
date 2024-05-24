:- module(tdatrain_21254766_AninatNorambuena,
         [train/6, trainAddCar/4, trainRemoveCar/3, isTrain/1, trainCapacity/2]).
:- use_module(utilidades_21254766_AninatNorambuena).
:- use_module(tdapcar_21254766_AninatNorambuena).
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

% Selectores
%
% Descripcion:
% Dominio:
% Predicado:
% Metas:
% Submetas:
% Clausulas:
getTrainId([Id,_,_,_,_], Id).

% Descripcion:
% Dominio:
% Predicado:
% Metas:
% Submetas:
% Clausulas:
getTrainMaker([_,Maker,_,_,_], Maker).

% Descripcion:
% Dominio:
% Predicado:
% Metas:
% Submetas:
% Clausulas:
getTrainRailType([_,_,RailType,_,_], RailType).

% Descripcion:
% Dominio:
% Predicado:
% Metas:
% Submetas:
% Clausulas:
getTrainSpeed([_,_,_,Speed,_], Speed).

% Descripcion:
% Dominio:
% Predicado:
% Metas:
% Submetas:
% Clausulas:
getTrainPcar([_,_,_,_,Pcars], Pcars).

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
    agregarElemListaIndice(ListaPcar, Pcar, Position, NuevaListaPcar),
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
    getTrainPcar(Train, TrainPcars),
    number(TrainId),
    string(TrainMaker),
    string(TrainRailType),
    number(TrainSpeed),
    TrainSpeed >= 0,
    carrosValidos(TrainPcars).

% Auxiliares
%
% Descripcion:
% Dominio:
% Predicado:
% Metas:
% Submetas:
% Clausulas:
capacidadTren([],0).
capacidadTren([H|T], Capacidad):-
    getPcarCapacity(H, Cap1),
    capacidadTren(T, Cap2),
    Capacidad = Cap1 + Cap2.

% RF14: trainCapacity
%
% Descripcion:
% Dominio:
% Predicado:
% Metas:
% Submetas:
% Clausulas:
trainCapacity(Train, Capacity):-
    getTrainPcar(Train, TrainPcars),
    capacidadTren(TrainPcars, Capacity).
