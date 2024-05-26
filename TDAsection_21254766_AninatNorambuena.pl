:- module(tdasection_21254766_AninatNorambuena,
         [section/5, section/1, listaSections/1, cortarListaSections/4, getSectionPoint1/2, getSectionPoint2/2, getSectionDistance/2, getSectionCost/2, comprobacionIdYNombre/4, comprobacionIdYNombre/1]).
:- use_module("utilidades_21254766_AninatNorambuena").
:- use_module("TDAstation_21254766_AninatNorambuena").
%TDA section

% Constructor
/*
%RF3: section

Descripcion: Predicado que permite establecer enlaces entre dos estaciones.
Dominio: Point1(station), Point2(station), Distance(int)>=0,
Cost(int)>=0
Predicado: section(Point1, Point2, Distance, Cost, [Point1, Point2, Distance, Cost])
Metas: section/5
Submetas: station/1, number/1
Clausulas:
*/
section(Point1, Point2, Distance, Cost, [Point1, Point2, Distance, Cost]):-
    station(Point1),
    station(Point2),
    Point1 \= Point2,
    number(Distance),
    Distance >= 0,
    number(Cost),
    Cost >= 0.

% Selectores
/*
Descripcion: Predicado que extrae el Point1 de una seccion
Dominio: Point1(station)
Predicado: getSectionPoint1([Point1,_,_,_], Point1)
Metas: getSectionPoint1/2
Submetas:
Clausulas:
*/
getSectionPoint1([Point1,_,_,_], Point1).

/*
Descripcion: Predicado que extrae el Point2 de una seccion
Dominio: Point2(station)
Predicado: getSectionPoint2([_,Point2,_,_], Point2)
Metas: getSectionPoint2/2
Submetas:
Clausulas:
*/
getSectionPoint2([_,Point2,_,_], Point2).

/*
Descripcion: Predicado que extrae la distancia de una seccion
Dominio: Distance(int)
Predicado: getSectionDistance([_,_, Distance,_], Distance)
Metas: getSectionDistance/2
Submetas:
Clausulas:
*/
getSectionDistance([_,_, Distance,_], Distance).

/*
Descripcion: Predicado que extrae el costo de una seccion
Dominio: Cost(int)
Predicado: getSectionCost([_,_,_,Cost], Cost)
Metas: getSectionCost/2
Submetas:
Clausulas:
*/
getSectionCost([_,_,_,Cost], Cost).

% Pertenencia
/*
Descripcion: Predicado que comprueba la validez de una seccion
Dominio: Point1(station), Point2(station), Distance(int), Cost(int)
Predicado: section([Point1, Point2, Distance, Cost])
Metas: section/1
Submetas: station/1, number/1
Clausulas:
*/
section([Point1, Point2, Distance, Cost]):-
    station(Point1),
    station(Point2),
    number(Distance),
    number(Cost).

% Predicados auxiliares
/*
Descripcion: Predicado que comprueba que todos los elementos de una
lista sean secciones
Dominio: H(cabeza de lista), T(cola de lista)
Predicado: listaSections([])
listaSections([H|_])
listaSections([_|T])
Metas: listaSecions/1
Submetas: section/1, listaSecions/1(recursividad)
Clausulas:
*/
listaSections([]).
listaSections([H|_]):- section(H).
listaSections([_|T]):- listaSections(T).

/*
Descripcion: Predicado que obtiene una lista de todas las primeras
estaciones de una lista de secciones
Dominio: H(cabeza de lista), T(cola de lista), ListaPoint1(station list)
Predicado: getListaPoint1([],[])
getListaPoint1([H|T], ListaPoint1)
Metas: getListaPoint1/2
Submetas: getSectionPoint1/2, getListaPoint1/2(recursividad)
Clausulas:
*/
getListaPoint1([],[]).
getListaPoint1([H|T], ListaPoint1):-
    getSectionPoint1(H, Point1),
    getListaPoint1(T, SubLista),
    ListaPoint1 = [Point1|SubLista].

/*
Descripcion: Predicado que obtiene una lista de todas las segundas
estaciones de una lista de secciones
Dominio: H(cabeza de lista), T(cola de lista), ListaPoint2(station list)
Predicado: getListaPoint2([],[])
getListaPoint2([H|T], ListaPoint2)
Metas: getListaPoint2/2
Submetas: getSectionPoint2/2, getListaPoint2/2(recursividad)
Clausulas:
*/
getListaPoint2([],[]).
getListaPoint2([H|T], ListaPoint2):-
    getSectionPoint2(H, Point2),
    getListaPoint2(T, SubLista),
    ListaPoint2 = [Point2|SubLista].

/*
Descripcion: Predicado que identifica una seccion en base a su primera
estacion
Dominio: H(cabeza de lista), T(cola de lista), Estacion(station),
Section(section)
Predicado: encontrarSection1([H|T], Estacion, Section)
encontrarSection1([H|_], Estacion, Section)
Metas: encontrarSection1/3
Submetas: getSectionPoint1/2, encontrarSection1/3(recursividad)
Clausulas:
*/
encontrarSection1([H|T], Estacion, Section):-
    getSectionPoint1(H, Estacion1),
    Estacion \= Estacion1,
    encontrarSection1(T, Estacion, Section).
encontrarSection1([H|_], Estacion, Section):-
    getSectionPoint1(H, Estacion1),
    Estacion = Estacion1,
    H = Section.

/*
Descripcion: Predicado que identifica una seccion en base a su segunda
estacion
Dominio: H(cabeza de lista), T(cola de lista), Estacion(station),
Section(section)
Predicado: encontrarSection2([H|T], Estacion, Section)
encontrarSection2([H|_], Estacion, Section)
Metas: encontrarEstacion/2
Submetas: getSectionPoint2/2, encontrarSection2/3(recursividad)
Clausulas:
*/
encontrarSection2([H|T], Estacion, Section):-
    getSectionPoint2(H, Estacion2),
    Estacion \= Estacion2,
    encontrarSection2(T, Estacion, Section).
encontrarSection2([H|_], Estacion, Section):-
    getSectionPoint2(H, Estacion2),
    Estacion = Estacion2,
    H = Section.

/*
Descripcion: Predicado que corta una lista de secciones en base a los
nombres de dos de sus estaciones
Dominio: ListaSections(section list), Nombre1(string), Nombre2(string),
NuevaListaSections(section list)
Predicado: cortarListaSections(ListaSections, Nombre1, Nombre2, NuevaListaSections)
Metas: cortarListaSections/4
Submetas: getListaPoint1/2, getListaPoint2/2, encontrarEstacion/3,
encontrarSection1/3, encontrarSection2/3, cortarListaInferior/3,
cortarListaSuperior/3
Clausulas:
*/
cortarListaSections(ListaSections, Nombre1, Nombre2, NuevaListaSections):-
    getListaPoint1(ListaSections, ListaPoint1),
    getListaPoint2(ListaSections, ListaPoint2),
    encontrarEstacion(ListaPoint1, Nombre1, Station1),
    encontrarEstacion(ListaPoint2, Nombre2, Station2),
    encontrarSection1(ListaSections, Station1, Section1),
    encontrarSection2(ListaSections, Station2, Section2),
    cortarListaInferior(ListaSections, Section1, ListaSections1),
    cortarListaSuperior(ListaSections1, Section2, NuevaListaSections).

/*
Descripcion: Comprueba la no repeticion de IDs en las estaciones de
una lista de secciones
Dominio: H(cabeza de lista), T(cola de lista), ListaIDs(int list),
ListaNombres(string list), EstacionesAnteriores(station list)
Predicado: comprobacionIdYNombre([],_,_,_)
comprobacionIdYNombre([H|T], ListaIDs, ListaNombres, EstacionesAnteriores)
comprobacionIdYNombre([H|T])
Metas: comprobacionIdYNombre/4, comprobacionIdYNombre/1
Submetas: getSectionPoint1/2, getStationId/2, getStationName/2,
member/2, not/1, getSectionPoint2/2, append/3, comprobacionIDYNombre/4
Clausulas:
*/
comprobacionIdYNombre([],_,_,_).
comprobacionIdYNombre([H|T], ListaIDs, ListaNombres, EstacionesAnteriores):-
    getSectionPoint1(H, Estacion1),
    getStationId(Estacion1, Id1),
    getStationName(Estacion1, Nombre1),
    ((not(member(Id1, ListaIDs)), not(member(Nombre1, ListaNombres)));
    member(Estacion1, EstacionesAnteriores)),
    getSectionPoint2(H, Estacion2),
    getStationId(Estacion2, Id2),
    getStationName(Estacion2, Nombre2),
    ((not(member(Id2, ListaIDs)), not(member(Nombre2, ListaNombres)));
    member(Estacion2, EstacionesAnteriores)),
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
