:- use_module(library(ordsets)).

build(and([A,B]), [NodeSubset, EdgeSubset]) :-
  NodeList = [arg(A), arg(B), arg(neg(A)), arg(neg(B))],
  EdgeList = [support(A,B), support(B,A),
              att(A,neg(A)), att(neg(A), A),
              att(B,neg(B)), att(neg(B), B),
              att(B,neg(A)),
              att(A,neg(B))],
  list_to_ord_set(NodeList, NodeSubset),
  list_to_ord_set(EdgeList, EdgeSubset).

build(nand([A,B]), [NodeSubset, EdgeSubset]) :-
  NodeList = [arg(A), arg(B), arg(neg(A)), arg(neg(B))],
  EdgeList = [att(A,B), att(B,A),
              att(A, neg(A)), att(neg(A), A),
              att(B,neg(B)), att(neg(B), B),
              support(B, neg(A)),
              support(A, neg(B))],
  list_to_ord_set(NodeList, NodeSubset),
  list_to_ord_set(EdgeList, EdgeSubset).

build(or([A,B]), [NodeSubset, EdgeSubset]) :-
  NodeList = [arg(A), arg(B), arg(neg(A)), arg(neg(B))],
  EdgeList = [att(A, neg(A)), att(neg(A), A),
              att(B, neg(B)), att(neg(B), B),
              att(neg(A), neg(B)), att(neg(B), neg(A)),
              support(neg(A), B),
              support(neg(B), A)],
  list_to_ord_set(NodeList, NodeSubset),
  list_to_ord_set(EdgeList, EdgeSubset).

build(xor([A,B]), [NodeSubset, EdgeSubset]) :-
  NodeList = [arg(A), arg(B), arg(neg(A)), arg(neg(B))],
  EdgeList = [att(A, neg(A)), att(neg(A), A),
              att(B, neg(B)), att(neg(B), B),
              att(A, B), att(B, A),
              support(neg(A), B), support(B, neg(A)),
              support(neg(B), A), support(A, neg(B))],
  list_to_ord_set(NodeList, NodeSubset),
  list_to_ord_set(EdgeList, EdgeSubset).

build(nor([A,B]), [NodeSubset, EdgeSubset]) :-
  NodeList = [arg(A), arg(B), arg(neg(A)), arg(neg(B))],
  EdgeList = [att(A, neg(A)), att(neg(A), A),
              att(B, neg(B)), att(neg(B), B),
              support(neg(A), neg(B)), support(neg(B), neg(A)),
              att(neg(B), A),
              att(neg(A), B)],
  list_to_ord_set(NodeList, NodeSubset),
  list_to_ord_set(EdgeList, EdgeSubset).

build(iff([A,B]), [NodeSubset, EdgeSubset]) :-
  NodeList = [arg(A), arg(B), arg(neg(A)), arg(neg(B))],
  EdgeList = [support(A, B), support(B,A),
              support(neg(A), neg(B)), support(neg(B), neg(A)),
              att(A,neg(A)), att(neg(A),A),
              att(B, neg(B)), att(neg(B), B),
              att(A, neg(B)), att(neg(B), A),
              att(B, neg(A)), att(neg(A), B)],
  list_to_ord_set(NodeList, NodeSubset),
  list_to_ord_set(EdgeList, EdgeSubset).


build(if([A,B]), [NodeSubset, EdgeSubset]) :-
  NodeList = [arg(A), arg(B), arg(neg(A)), arg(neg(B))],
  EdgeList = [support(A,B),
              support(neg(B), neg(A)),
              att(A,neg(A)), att(neg(A),A),
              att(B, neg(B)), att(neg(B), B),
              att(A, neg(B)), att(neg(B), A)],
  list_to_ord_set(NodeList, NodeSubset),
  list_to_ord_set(EdgeList, EdgeSubset).