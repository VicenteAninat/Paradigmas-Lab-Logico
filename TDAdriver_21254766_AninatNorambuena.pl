:- module(tdadriver_21254766_AninatNorambuena,
         [driver/4]).
% TDA driver

% Constructor
/*
RF15: driver

Descripcion: Predicado que permite crear un conductor cuya habilitación
de conducción depende del fabricante de tren (train-maker)
Dominio: Id(int), Nombre(string), TrainMaker(string)
Predicado: driver(Id, Nombre, TrainMaker, [Id, Nombre, TrainMaker])
Metas: driver/4
Submetas: number/1, string/1
Clausulas:
*/
driver(Id, Nombre, TrainMaker, [Id, Nombre, TrainMaker]):-
    number(Id),
    string(Nombre),
    string(TrainMaker).
