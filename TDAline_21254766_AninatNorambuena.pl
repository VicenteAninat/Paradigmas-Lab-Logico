:- consult("TDAstation_21254766_AninatNorambuena.pl").
:- consult("TDAsection_21254766_AninatNorambuena.pl").
%TDA line

% Constructor
% RF4: line
%
% Descripcion:
% Dominio:
% Predicado:
% Metas:
% Submetas:
% Clausulas:
line(Id, Name, RailType, Sections, [Id, Name, RailType, ListaSectionsSinDup]):-
    number(Id),
    string(Name),
    string(RailType),
    listaSections(Sections),
    deleteDuplicate(Sections, ListaSectionsSinDup).

% Selectores
% Descripcion:
% Dominio:
% Predicado:
% Metas:
% Submetas:
% Clausulas:
getLineId([Id,_,_,_], Id).

% Descripcion:
% Dominio:
% Predicado:
% Metas:
% Submetas:
% Clausulas:
getLineName([_,Name,_,_], Name).

% Descripcion:
% Dominio:
% Predicado:
% Metas:
% Submetas:
% Clausulas:
getLineRailType([_,_,RailType,_], RailType).

% Descripcion:
% Dominio:
% Predicado:
% Metas:
% Submetas:
% Clausulas:
getLineSections([_,_,_,Sections], Sections).

% Auxiliares
%
% Descripcion:
% Dominio:
% Predicado:
% Metas:
% Submetas:
% Clausulas:
lineLargo([], 0).
lineLargo([_|T], Length):-
    lineLargo(T, Largo1),
    Length is 1 + Largo1.

% Descripcion:
% Dominio:
% Predicado:
% Metas:
% Submetas:
% Clausulas:
lineDistancia([], 0).
lineDistancia([H|T], Distance):-
    getSectionDistance(H, Distancia1),
    lineDistancia(T, Distancia2),
    Distance is Distancia1 + Distancia2.

% Descripcion:
% Dominio:
% Predicado:
% Metas:
% Submetas:
% Clausulas:
lineCosto([], 0).
lineCosto([H|T], Cost):-
    getSectionCost(H, Costo1),
    lineCosto(T, Costo2),
    Cost is Costo1 + Costo2.

% RF5: lineLength
%
% Descripcion:
% Dominio:
% Predicado:
% Metas:
% Submetas:
% Clausulas:
lineLength(Line, Length, Distance, Cost):-
    getLineSections(Line, ListaSection),
    lineLargo(ListaSection, Length),
    lineDistancia(ListaSection, Distance),
    lineCosto(ListaSection, Cost).
