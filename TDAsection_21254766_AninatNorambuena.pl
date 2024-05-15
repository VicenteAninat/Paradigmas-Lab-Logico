:- consult("TDAstation_21254766_AninatNorambuena.pl").
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
