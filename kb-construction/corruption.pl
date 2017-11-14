:- use_module(library(lists)).
:- use_module(library(random)).

atom_list_contamination(AtomList, Names, BodyPerc, Res) :-
	random(BodyRandom),
	atom_list_contamination_loop(AtomList, Names, BodyRandom, BodyPerc, [], Res).

atom_list_contamination_loop([], Names, BodyRandom, BodyPerc, Acc, Acc).

atom_list_contamination_loop([H|T], Names, BodyRandom, BodyPerc, Acc, Res) :-
	BodyRandom =< BodyPerc,
  single_contamination(H, Names, NewH),
	append(Acc, [NewH], NewAcc),
	random(NewBodyRandom),
	atom_list_contamination_loop(T, Names, NewBodyRandom, BodyPerc, NewAcc, Res).

atom_list_contamination_loop([H|T], Names, BodyRandom, BodyPerc, Acc, Res) :-
	BodyRandom > BodyPerc,
	append(Acc, [H], NewAcc),
	random(NewBodyRandom),
	atom_list_contamination_loop(T, Names, NewBodyRandom, BodyPerc, NewAcc, Res).

single_contamination(male(X), Names, female(X)).

single_contamination(female(X), Names, male(X)).

single_contamination(married(A,B), Names, NewMarried) :-
	random_member(Z, Names),
  random_member(NewMarried, [married(B,A), married(A,Z), married(Z,B), parent(A,B), parent(B,A)]).

single_contamination(parent(A,B), Names, NewParent) :-
	random_member(Z, Names),
  random_member(NewParent, [parent(B,A), parent(A,Z), parent(Z,B), married(A,B), married(B,A)]).

random_member(D, A) :-
  length(A, B),
  C is random(B),
  nth0(C, A, D).
