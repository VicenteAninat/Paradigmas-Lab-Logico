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
    number(Cost).

% Selectores
% Descripcion:
% Dominio:
% Predicado:
% Metas:
% Submetas:
% Clausulas:
getSectionDistance([_,_, Distance,_], Distance).

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
