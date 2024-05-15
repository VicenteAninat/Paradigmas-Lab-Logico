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
    number(StopTime),
    StopTime >= 0.

% Selectores
%
% Descripcion:
% Dominio:
% Predicado:
% Metas:
% Submetas:
% Clausulas:
getStationName([_,Name,_,_], Name).

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

% Descripcion:
% Dominio:
% Predicado:
% Metas:
% Submetas:
% Clausulas:
cortarListaInferior([], _, []).
cortarListaInferior([H|T], Elem, NuevaLista):-
    H \= Elem,
    cortarListaInferior(T, Elem, NuevaLista).
cortarListaInferior([H|T], Elem, NuevaLista):-
    H = Elem,
    NuevaLista = [H|T].

% Descripcion:
% Dominio:
% Predicado:
% Metas:
% Submetas:
% Clausulas:
cortarListaSuperior([], _, []).
cortarListaSuperior([H|T], Elem, NuevaLista):-
    H \= Elem,
    cortarListaSuperior(T, Elem, NuevaLista2),
    NuevaLista = [H|NuevaLista2].
cortarListaSuperior([H|_], Elem, NuevaLista):-
    H = Elem,
    NuevaLista = [H].

% Descripcion:
% Dominio:
% Predicado:
% Metas:
% Submetas:
% Clausulas:
encontrarEstacion([H|T], Nombre, Estacion):-
    getStationName(H, NombreEstacion),
    Nombre \= NombreEstacion,
    encontrarEstacion(T, Nombre, Estacion).
encontrarEstacion([H|_], Nombre, Estacion):-
    getStationName(H, NombreEstacion),
    Nombre = NombreEstacion,
    H = Estacion.

% Descripcion:
% Dominio:
% Predicado:
% Metas:
% Submetas:
% Clausulas:
listAdd([],Elem, [Elem]).
listAdd([H|T], Elem, NuevaLista):-
    listAdd(T, Elem, NuevaLista2),
    NuevaLista = [H, NuevaLista2].

% Descripcion:
% Dominio:
% Predicado:
% Metas:
% Submetas:
% Clausulas:
verificarAusencia([],_).
verificarAusencia([H|T], Elem):-
    H \= Elem,
    verificarAusencia(T, Elem).
