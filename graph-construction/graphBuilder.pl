:-  module(graphBuilder,[build_multi_graph/3]). 

:- consult('constraints.pl').
:- consult('subgraphBuilder.pl').
:- use_module(library(ordsets)).
:- use_module(library(lists)).


build_multi_graph([], FilteringList, []).

build_multi_graph([Body|Bodies], FilteringList, [Graph|Graphs]) :-
  build_graph(Body, FilteringList, Graph),
  build_multi_graph(Bodies, FilteringList, Graphs).

build_graph(BodyList, FilteringList, Graph) :-
  assert_body(BodyList),
  setof(X, ic(X), Constraints),
  constraint_filter(Constraints, FilteringList, FilteredConstraints),
  retract_body(BodyList),
  building(FilteredConstraints, Graph).

building([], [[],[]]).
building(FilteredConstraints, Graph) :-
  atom_evaluation(FilteredConstraints, [[],[]], Graph).
  

assert_body([]).

assert_body([H|T]) :-
  assert(H),
  assert_body(T).

retract_body([]).

retract_body([H|T]) :-
  retract(H),
  retract_body(T).


atom_evaluation([], Graph, Graph).

atom_evaluation([Vertex|RemainingVertexes], [Node, Edge], FinalGraph) :-
  build(Vertex, [SubNode, SubEdge]),
  ord_union(SubNode, Node, NewNode),
  ord_union(SubEdge, Edge, NewEdge),
  atom_evaluation(RemainingVertexes, [NewNode, NewEdge], FinalGraph).

% --------------------------------------------------------------------------------------------------------------------------------------------------------
constraint_filter(All, FilteringList, Res) :-
  constraint_filter_loop(All, FilteringList, [], Res).

constraint_filter_loop(All, [], Acc, Acc).

constraint_filter_loop(All, [and|ConstT], Acc, Res) :-
  and_filter(All, [], SubFiltered),
  append(Acc, SubFiltered, NewAcc),
  constraint_filter_loop(All, ConstT, NewAcc, Res).

constraint_filter_loop(All, [nand|ConstT], Acc, Res) :-
  nand_filter(All, [], SubFiltered),
  append(Acc, SubFiltered, NewAcc),
  constraint_filter_loop(All, ConstT, NewAcc, Res).

constraint_filter_loop(All, [or|ConstT], Acc, Res) :-
  or_filter(All, [], SubFiltered),
  append(Acc, SubFiltered, NewAcc),
  constraint_filter_loop(All, ConstT, NewAcc, Res).

constraint_filter_loop(All, [nor|ConstT], Acc, Res) :-
  nor_filter(All, [], SubFiltered),
  append(Acc, SubFiltered, NewAcc),
  constraint_filter_loop(All, ConstT, NewAcc, Res).

constraint_filter_loop(All, [xor|ConstT], Acc, Res) :-
  xor_filter(All, [], SubFiltered),
  append(Acc, SubFiltered, NewAcc),
  constraint_filter_loop(All, ConstT, NewAcc, Res).

constraint_filter_loop(All, [if|ConstT], Acc, Res) :-
  if_filter(All, [], SubFiltered),
  append(Acc, SubFiltered, NewAcc),
  constraint_filter_loop(All, ConstT, NewAcc, Res).

constraint_filter_loop(All, [iff|ConstT], Acc, Res) :-
  iff_filter(All, [], SubFiltered),
  append(Acc, SubFiltered, NewAcc),
  constraint_filter_loop(All, ConstT, NewAcc, Res).

constraint_filter_loop(All, [H|ConstT], Acc, Res) :-
  constraint_filter_loop(All, ConstT, Acc, Res).


and_filter([], Acc, Acc).

and_filter([and(X)|T], Acc, Res) :-
  append(Acc, [and(X)], NewAcc),
  and_filter(T, NewAcc, Res).

and_filter([H|T], Acc, Res) :-
  and_filter(T, Acc, Res).


nand_filter([], Acc, Acc).

nand_filter([nand(X)|T], Acc, Res) :-
  append(Acc, [nand(X)], NewAcc),
  nand_filter(T, NewAcc, Res).

nand_filter([H|T], Acc, Res) :-
  nand_filter(T, Acc, Res).


or_filter([], Acc, Acc).

or_filter([or(X)|T], Acc, Res) :-
  append(Acc, [or(X)], NewAcc),
  or_filter(T, NewAcc, Res).

or_filter([H|T], Acc, Res) :-
  or_filter(T, Acc, Res).


nor_filter([], Acc, Acc).

nor_filter([nor(X)|T], Acc, Res) :-
  append(Acc, [nor(X)], NewAcc),
  nor_filter(T, NewAcc, Res).

nor_filter([H|T], Acc, Res) :-
  nor_filter(T, Acc, Res).


xor_filter([], Acc, Acc).

xor_filter([xor(X)|T], Acc, Res) :-
  append(Acc, [xor(X)], NewAcc),
  xor_filter(T, NewAcc, Res).

xor_filter([H|T], Acc, Res) :-
  xor_filter(T, Acc, Res).


if_filter([], Acc, Acc).

if_filter([if(X)|T], Acc, Res) :-
  append(Acc, [if(X)], NewAcc),
  if_filter(T, NewAcc, Res).

if_filter([H|T], Acc, Res) :-
  if_filter(T, Acc, Res).


iff_filter([], Acc, Acc).

iff_filter([iff(X)|T], Acc, Res) :-
  append(Acc, [iff(X)], NewAcc),
  iff_filter(T, NewAcc, Res).

iff_filter([H|T], Acc, Res) :-
  iff_filter(T, Acc, Res).
% ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
