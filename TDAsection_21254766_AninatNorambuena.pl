:- module(tdasection_21254766_AninatNorambuena,
         [listaSections/1, cortarListaSections/4, getSectionPoint1/2, getSectionPoint2/2, getSectionDistance/2, getSectionCost/2, comprobacionIdYNombre/3, comprobacionIdYNombre/1]).
:- use_module("utilidades_21254766_AninatNorambuena").
:- use_module("TDAstation_21254766_AninatNorambuena").
%TDA section

% Constructor
%RF3: section
%
% Descripcion:
% Dominio:
% Predicado:
% Metas:
% Submetas:
% Clausulas:
section(Point1, Point2, Distance, Cost, [Point1, Point2, Distance, Cost]):-
    station(Point1),
    station(Point2),
    Point1 \= Point2,
    number(Distance),
    Distance >= 0,
    number(Cost),
    Cost >= 0.

% Selectores
%
% Descripcion:
% Dominio:
% Predicado:
% Metas:
% Submetas:
% Clausulas:
getSectionPoint1([Point1,_,_,_], Point1).

% Descripcion:
% Dominio:
% Predicado:
% Metas:
% Submetas:
% Clausulas:
getSectionPoint2([_,Point2,_,_], Point2).

% Descripcion:
% Dominio:
% Predicado:
% Metas:
% Submetas:
% Clausulas:
getSectionDistance([_,_, Distance,_], Distance).

% Selectores
% Descripcion:
% Dominio:
% Predicado:
% Metas:
% Submetas:
% Clausulas:
getSectionCost([_,_,_,Cost], Cost).

% Pertenencia
%
% Descripcion:
% Dominio:
% Predicado:
% Metas:
% Submetas:
% Clausulas:
section([Point1, Point2, Distance, Cost]):-
    station(Point1),
    station(Point2),
    number(Distance),
    number(Cost).

% Predicados auxiliares
%
% Descripcion:
% Predicado:
% Metas:
% Submetas:
% Clausulas:
listaSections([]).
listaSections([H|_]):- section(H).
listaSections([_|T]):- listaSections(T).

% Descripcion:
% Predicado:
% Metas:
% Submetas:
% Clausulas:
getListaPoint1([],[]).
getListaPoint1([H|T], ListaPoint1):-
    getSectionPoint1(H, Point1),
    getListaPoint1(T, SubLista),
    ListaPoint1 = [Point1|SubLista].

% Descripcion:
% Predicado:
% Metas:
% Submetas:
% Clausulas:
getListaPoint2([],[]).
getListaPoint2([H|T], ListaPoint2):-
    getSectionPoint2(H, Point2),
    getListaPoint2(T, SubLista),
    ListaPoint2 = [Point2|SubLista].

% Descripcion:
% Predicado:
% Metas:
% Submetas:
% Clausulas:
encontrarSection1([H|T], Estacion, Section):-
    getSectionPoint1(H, Estacion1),
    Estacion \= Estacion1,
    encontrarSection1(T, Estacion, Section).
encontrarSection1([H|_], Estacion, Section):-
    getSectionPoint1(H, Estacion1),
    Estacion = Estacion1,
    H = Section.

% Descripcion:
% Predicado:
% Metas:
% Submetas:
% Clausulas:
encontrarSection2([H|T], Estacion, Section):-
    getSectionPoint2(H, Estacion2),
    Estacion \= Estacion2,
    encontrarSection2(T, Estacion, Section).
encontrarSection2([H|_], Estacion, Section):-
    getSectionPoint2(H, Estacion2),
    Estacion = Estacion2,
    H = Section.

% Descripcion:
% Predicado:
% Metas:
% Submetas:
% Clausulas:
cortarListaSections(ListaSections, Nombre1, Nombre2, NuevaListaSections):-
    getListaPoint1(ListaSections, ListaPoint1),
    getListaPoint2(ListaSections, ListaPoint2),
    encontrarEstacion(ListaPoint1, Nombre1, Station1),
    encontrarEstacion(ListaPoint2, Nombre2, Station2),
    encontrarSection1(ListaSections, Station1, Section1),
    encontrarSection2(ListaSections, Station2, Section2),
    cortarListaInferior(ListaSections, Section1, ListaSections1),
    cortarListaSuperior(ListaSections1, Section2, NuevaListaSections).

% Descripcion: Comprueba la no repeticion de IDs en las estaciones de
% una lista de secciones
% Predicado:
% Metas:
% Submetas:
% Clausulas:
comprobacionIdYNombre([],_,_,_).
comprobacionIdYNombre([H|T], ListaIDs, ListaNombres, EstacionesAnteriores):-
    getSectionPoint1(H, Estacion1),
    getStationId(Estacion1, Id1),
    getStationName(Estacion1, Nombre1),
    (not(member(Id1, ListaIDs)), not(member(Nombre1, ListaNombres)));
    member(Estacion1, EstacionesAnteriores),
    getSectionPoint2(H, Estacion2),
    getStationId(Estacion2, Id2),
    getStationName(Estacion2, Nombre2),
    (not(member(Id2, ListaIDs)), not(member(Nombre2, ListaNombres)));
    member(Estacion2, EstacionesAnteriores),
    append(ListaIDs, [Id1, Id2], NuevaListaIDs),
    append(EstacionesAnteriores, [Estacion1, Estacion2], NuevaEstacionesAnteriores),
    append(ListaNombres, [Nombre1, Nombre2], NuevaListaNombres),
    comprobacionIdYNombre(T, NuevaListaIDs, NuevaListaNombres, NuevaEstacionesAnteriores).
comprobacionIdYNombre([H|T]):-
    getSectionPoint1(H, Estacion1),
    getStationId(Estacion1, Id1),
    getStationName(Estacion1, Nombre1),
    getSectionPoint2(H, Estacion2),
    getStationId(Estacion2, Id2),
    getStationName(Estacion2, Nombre2),
    ListaIDs = [Id1, Id2],
    ListaNombres = [Nombre1, Nombre2],
    ListaEstacionesAnteriores = [Estacion1, Estacion2],
    comprobacionIdYNombre(T, ListaIDs, ListaNombres, ListaEstacionesAnteriores).
