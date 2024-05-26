:- module(tdatrain_21254766_AninatNorambuena,
         [train/6, trainAddCar/4, trainRemoveCar/3, isTrain/1, trainCapacity/2]).
:- use_module(utilidades_21254766_AninatNorambuena).
:- use_module(tdapcar_21254766_AninatNorambuena).
% TDA train

% Constructor
/*
RF10: train

Descripcion: Predicado que permite crear un tren o convoy.
Dominio: id(int), Maker(string), RailType(string), Speed(int),
Pcars(pcar list)
Predicado: train(Id, Maker, RailType, Speed, Pcars, [Id, Maker, RailType, Speed, Pcars])
Metas: train/6
Submetas: number/1, string/1
Clausulas:
*/
train(Id, Maker, RailType, Speed, Pcars, [Id, Maker, RailType, Speed, Pcars]):-
    number(Id),
    string(Maker),
    string(RailType),
    number(Speed),
    Speed >= 0.

% Selectores
/*
Descripcion: Predicado que extrae el Id de un tren
Dominio: Id(int)
Predicado: getTrainId([Id,_,_,_,_], Id)
Metas: getTrainId/2
Submetas:
Clausulas:
*/
getTrainId([Id,_,_,_,_], Id).

/*
Descripcion: Predicado que extrae el maker de un tren
Dominio: Maker(string)
Predicado: getTrainMaker([_,Maker,_,_,_], Maker)
Metas: getTrainMaker/2
Submetas:
Clausulas:
*/
getTrainMaker([_,Maker,_,_,_], Maker).

/*
Descripcion: Predicado que extrae el tipo de rail de un tren
Dominio: RailType(string)
Predicado: getTrainRailType([_,_,RailType,_,_], RailType)
Metas: getTrainRailType/2
Submetas:
Clausulas:
*/
getTrainRailType([_,_,RailType,_,_], RailType).

/*
Descripcion: Predicado que extrae la velocidad de un tren
Dominio: Speed(int)
Predicado: getTrainSpeed([_,_,_,Speed,_], Speed)
Metas: getTrainSpeed/2
Submetas:
Clausulas:
*/
getTrainSpeed([_,_,_,Speed,_], Speed).

/*
Descripcion: Predicado que extrae los carros de un tren
Dominio: Pcars(pcar list)
Predicado: getTrainPcar([_,_,_,_,Pcars], Pcars)
Metas: getTrainPcar/2
Submetas:
Clausulas:
*/
getTrainPcar([_,_,_,_,Pcars], Pcars).

% Modificadores
/*
RF11: trainAddCar

Descripcion: Función que permite añadir carros a un tren en una posición dada.
Dominio: Train(train), Pcar(pcar), Position(int), TrainOut(train)
Predicado: trainAddCar(Train, Pcar, Position, TrainOut)
Metas: trainAddCar/4
Submetas: number/1, getTrainPcar/2, verificarAusencia/2,
verificarPosicion/2, agregarElemListaIndice/4, getTrainId/2,
getTrainMaker/2, getTrainRailType/2, getTrainSpeed/2
Clausulas:
*/
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

/*
RF12: trainRemoveCar

Descripcion: Predicado que permite eliminar un carro desde el convoy.
Dominio: Train(train), Position(int), TrainOut(train)
Predicado: trainRemoveCar(Train, Position, TrainOut)
Metas: trainRemoveCar/3
Submetas: number/1, getTrainPcar/2, verificarAusencia/2,
verificarPosicion/2, eliminarElemListaIndice/3, getTrainId/2,
getTrainMaker/2, getTrainRailType/2, getTrainSpeed/2
Clausulas:
*/
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
/*
RF13: isTrain

Descripcion: Predicado que permite determinar si un elemento es un tren
válido, esto es, si el elemento tiene la estructura de tren y los carros
que lo conforman son compatibles (mismo modelo) y tienen una disposición
coherente con carros terminales (tr) en los extremos y centrales (ct) en
medio del convoy.
Dominio: Train(train)
Predicado: isTrain(Train)
Metas: isTrain/1
Submetas: getTrainId/2, getTrainMaker/2, getTrainRailType/2,
getTrainSpeed/2, getTrainPcar/2, number/1, string/1
Clausulas:
*/
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
/*
Descripcion: Predicado que calcula la capacidad total de una lista de
carros
Dominio: H(cabeza de lista), T(cola de lista), Capacidad(int)
Predicado: capacidadTren([],0)
capacidadTren([H|T], Capacidad)
Metas: capacidadTren/2
Submetas: getPcarCapacity/2, capacidadTren/2(recursividad)
Clausulas:
*/
capacidadTren([],0).
capacidadTren([H|T], Capacidad):-
    getPcarCapacity(H, Cap1),
    capacidadTren(T, Cap2),
    Capacidad is Cap1 + Cap2.

/*
RF14: trainCapacity

Descripcion: Predicado que permite determinar la capacidad máxima de pasajeros del tren.
Dominio: Train(train), Capacity(int)
Predicado: trainCapacity(Train, Capacity)
Metas: trainCapacity/2
Submetas: getTrainPcar/2, capacidadTren/2
Clausulas:
*/
trainCapacity(Train, Capacity):-
    getTrainPcar(Train, TrainPcars),
    capacidadTren(TrainPcars, Capacity).
