%TDA station

% Descripcion:
% Dominio:
% Predicado:
% Metas:
% Submetas:
% Clausulas:
type(X):-
    X = "c";
    X = "m";
    X = "r";
    X = "t".

% Constructor
% RF2: station
%
% Descripcion:
% Dominio:
% Predicado:
% Metas:
% Submetas:
% Clausulas:
station(Id, Name, Type, StopTime, [Id, Name, Type, StopTime]):-
    number(Id),
    string(Name),
    type(Type),
    number(StopTime).

% Pertenencia
%
% Descripcion:
% Dominio:
% Predicado:
% Metas:
% Submetas:
% Clausulas:
station([Id, Name, Type, StopTime]):-
    number(Id),
    string(Name),
    type(Type),
    number(StopTime).

% Predicados auxiliares
% Descripcion:
% Dominio:
% Predicado:
% Metas:
% Submetas:
% Clausulas:
objetivo(X, Y):- X = Y.
filtrar(Elem, Lista, R):- exclude(objetivo(Elem), Lista, R).

% Descripcion:
% Dominio:
% Predicado:
% Metas:
% Submetas:
% Clausulas:
deleteDuplicate([],[]). %Caso base
deleteDuplicate([H|L], R):- filtrar(H, L, LFiltrada), R=[H|Y], deleteDuplicate(LFiltrada, Y).
