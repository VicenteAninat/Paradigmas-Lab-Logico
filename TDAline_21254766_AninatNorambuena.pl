:- module(tdaline_21254766_AninatNorambuena,
         [line/5, lineLength/4, lineSectionLength/6, lineAddSection/3, isLine/1]).
:- use_module("utilidades_21254766_AninatNorambuena").
:- use_module("TDAstation_21254766_AninatNorambuena").
:- use_module("TDAsection_21254766_AninatNorambuena").
% TDA line

% Constructor
/*
RF4: line

Descripcion: Predicado que permite crear una línea
Dominio: Id(int), Name(string), RailType(string), Sections(section list)
Predicado: line(Id, Name, RailType, Sections, [Id, Name, RailType, ListaSectionsSinDup])
Metas: line/5
Submetas: number/1, string/1, listaSections/1, deleteDuplicate/2
Clausulas:
*/
line(Id, Name, RailType, Sections, [Id, Name, RailType, ListaSectionsSinDup]):-
    number(Id),
    string(Name),
    string(RailType),
    listaSections(Sections),
    deleteDuplicate(Sections, ListaSectionsSinDup).

% Selectores
/*
Descripcion: Predicado que extrae el Id de una linea
Dominio: Id(int)
Predicado: getLineId([Id,_,_,_], Id)
Metas: getLineId/2
Submetas:
Clausulas:
*/
getLineId([Id,_,_,_], Id).

/*
Descripcion: Predicado que extrae el nombre de una linea
Dominio: Name(string)
Predicado: getLineName([_,Name,_,_], Name)
Metas: getLineName/2
Submetas:
Clausulas:
*/
getLineName([_,Name,_,_], Name).

/*
Descripcion: Predicado que extrae el nombre de una linea
Dominio: RailType(string)
Predicado: getLineRailType([_,_,RailType,_], RailType)
Metas: getLineRailType/2
Submetas:
Clausulas:
*/
getLineRailType([_,_,RailType,_], RailType).

/*
Descripcion: Predicado que extrae el nombre de una linea
Dominio: Sections(section list)
Predicado: getLineSections([_,_,_,Sections], Sections)
Metas: getLineSections/2
Submetas:
Clausulas:
*/
getLineSections([_,_,_,Sections], Sections).

% Auxiliares
/*
Descripcion: Predicado que calcula el largo de una linea en base a su
cantidad de secciones
Dominio: T(cola de lista), Length(int)
Predicado: lineLargo([], 0)
lineLargo([_|T], Length)
Metas: lineLargo/2
Submetas: lineLargo/2(recursividad)
Clausulas:
*/
lineLargo([], 0).
lineLargo([_|T], Length):-
    lineLargo(T, Largo1),
    Length is 1 + Largo1.

/*
Descripcion: Predicado que calcula la distancia total de una linea en
base a las distancias de sus secciones
Dominio: H(cabeza de lista), T(cola de lista), Distance
Predicado: lineDistancia([], 0)
lineDistancia([H|T], Distance)
Metas: lineDistancia/2
Submetas: getSectionDistance/2, lineDistancia/2(recursividad)
Clausulas:
*/
lineDistancia([], 0).
lineDistancia([H|T], Distance):-
    getSectionDistance(H, Distancia1),
    lineDistancia(T, Distancia2),
    Distance is Distancia1 + Distancia2.

/*
Descripcion: Predicado que calcula el costo total de una linea en
base a los costos de sus secciones
Dominio: H(cabeza de lista), T(cola de lista), Cost(int)
Predicado: lineCosto([], 0)
lineCosto([H|T], Cost)
Metas: lineCosto/2
Submetas: getSectionCost/2, lineCosto/2(recursividad)
Clausulas:
*/
lineCosto([], 0).
lineCosto([H|T], Cost):-
    getSectionCost(H, Costo1),
    lineCosto(T, Costo2),
    Cost is Costo1 + Costo2.

/*
RF5: lineLength

Descripcion: Predicado que permite determinar el largo total de una
línea (cantidad de estaciones), la distancia (en la unidad de medida
expresada en cada tramo) y su costo.
Dominio: Line(line), Length(int), Distance(int), Cost(int)
Predicado: lineLength(Line, Length, Distance, Cost)
Metas: lineLength/4
Submetas: getLineSections/2, lineLargo/2, lineDistancia/2, lineCosto/2
Clausulas:
*/
lineLength(Line, Length, Distance, Cost):-
    getLineSections(Line, ListaSection),
    lineLargo(ListaSection, Length),
    lineDistancia(ListaSection, Distance),
    lineCosto(ListaSection, Cost).

/*
RF6: lineSectionLength

Descripcion: Predicado que permite determinar el trayecto entre una
estación origen y una final, la distancia de ese trayecto y el costo.
Dominio: Line(line), Station1Name(string), Station2Name(string),
Path(section list), Distance(int), Cost(int)
Predicado: lineSectionLength(Line, Station1Name, Station2Name, Path, Distance, Cost)
Metas: lineSectionLength/6
Submetas: getLineSections/2, cortarListaSections/4, lineDistancia/2,
lineCosto/2
Clausulas:
*/
lineSectionLength(Line, Station1Name, Station2Name, Path, Distance, Cost):-
    getLineSections(Line, ListaSections),
    cortarListaSections(ListaSections, Station1Name, Station2Name, Path),
    lineDistancia(Path, Distance),
    lineCosto(Path, Cost).

/*
RF7: lineAddSection

Descripcion: Predicado que permite añadir tramos a una línea
Dominio: Line(line), Section(section), LineOut(line)
Predicado: lineAddSection(Line, Section, LineOut)
Metas: lineAddSection/3
Submetas: getLineSections/2, verificarAusencia/2, listAdd/3,
getLineId/2, getLineName/2, getLineRailType/2
Clausulas:
*/
lineAddSection(Line, Section, LineOut):-
    getLineSections(Line, ListaSections),
    verificarAusencia(ListaSections, Section),
    listAdd(ListaSections, Section, NuevaListaSection),
    getLineId(Line, LineID),
    getLineName(Line, LineName),
    getLineRailType(Line, LineRailType),
    LineOut = [LineID, LineName, LineRailType, NuevaListaSection].

/*
Descripcion: Predicado que comprueba que los extremos de una linea
tengan estaciones terminales
Dominio: ListaSection(section list)
Predicado: comprobacionExtremosDeLinea(ListaSections)
Metas: comprobacionExtremosDeLinea/1
Submetas: last/2, reverse/2, getSectionPoint1/2, getSectionPoint2/2,
getStationType/2
Clausulas:
*/
comprobacionExtremosDeLinea(ListaSections):-
    last(ListaSections, UltimaSection),
    reverse(ListaSections, ListaSectionsInv),
    last(ListaSectionsInv, PrimeraSection),
    getSectionPoint1(PrimeraSection, PrimeraEstacion),
    getSectionPoint2(UltimaSection, UltimaEstacion),
    getStationType(PrimeraEstacion, PrimeraEstacionType),
    getStationType(UltimaEstacion, UltimaEstacionType),
    (PrimeraEstacionType == t; PrimeraEstacionType == c),
    (UltimaEstacionType == t; UltimaEstacionType == c).

/*
Descripcion: Predicado que comprueba que una linea sea circular en base
a la igualdad de su primera y su ultima estacion
Dominio: ListaSections(section list)
Predicado: comprobacionLineaCircular(ListaSections)
Metas: comprobacionLineaCircular/1
Submetas: last/2, reverse/2, getSectionPoint1/2, getSectionPoint2/2
Clausulas:
*/
comprobacionLineaCircular(ListaSections):-
    last(ListaSections, UltimaSection),
    reverse(ListaSections, ListaSectionsInv),
    last(ListaSectionsInv, PrimeraSection),
    getSectionPoint1(PrimeraSection, PrimeraEstacion),
    getSectionPoint2(UltimaSection, UltimaEstacion),
    PrimeraEstacion = UltimaEstacion.

/*
Descripcion: Predicado que comprueba que a partir de una estacion se
pueda llegar a todas las demas en una linea
Dominio: H(cabeza de lista), T(cola de lista), ListaEstaciones(station
list), EstacionAnterior(station)
Predicado: comprobacionLlegada([],_,_)
comprobacionLlegada([H|T], ListaEstaciones, EstacionAnterior)
comprobacionLlegada([H|T])
Metas: comprobacionLlegada/3
Submetas: getSectionPoint1/2, getSectionPoint2/2, member/2, append/3,
comprobacionLlegada/3(recursividad)
Clausulas:
*/
comprobacionLlegada([],_,_).
comprobacionLlegada([H|T], ListaEstaciones, EstacionAnterior):-
    getSectionPoint1(H, Estacion1),
    getSectionPoint2(H, Estacion2),
    (Estacion1 = EstacionAnterior;
    member(Estacion1, ListaEstaciones)),
    append(ListaEstaciones, [Estacion1, Estacion2], NuevaListaEstaciones),
    comprobacionLlegada(T, NuevaListaEstaciones, Estacion2).
comprobacionLlegada([H|T]):-
    getSectionPoint1(H, Estacion1),
    getSectionPoint2(H, Estacion2),
    ListaEstaciones = [Estacion1, Estacion2],
    comprobacionLlegada(T, ListaEstaciones, Estacion2).

/*
RF8: isLine

Descripcion: Predicado que permite determinar si un elemento cumple con
las restricciones señaladas en apartados anteriores en relación a las
estaciones y tramos para poder conformar una línea.
Dominio: Line(line)
Predicado: isLine(Line)
Metas: isLine/1
Submetas: getLineId/2, getLineName/2, getLineRailType/2,
getLineSections/2, number/1, string/1, comprobacionIdYNombre/1,
comprobacionExtremosDeLinea/1, comprobacionLineaCircular/1,
comprobacionLlegada/1
Clausulas:
*/
isLine(Line):-
    getLineId(Line, LineId),
    getLineName(Line, LineName),
    getLineRailType(Line, LineRailType),
    getLineSections(Line, LineSections),
    number(LineId),
    string(LineName),
    string(LineRailType),
    comprobacionIdYNombre(LineSections),
    (comprobacionExtremosDeLinea(LineSections);
    comprobacionLineaCircular(LineSections)),
    comprobacionLlegada(LineSections).
