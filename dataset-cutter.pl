/*
	Subset1
	[callie, carl, martha, mark, lorrie, leon, steve, susan, rachel, paul, paula, oma, own, m7, m6, m5, m4, m3, f2, m1]
*/


:- use_module(library(ordsets)).
:- use_module(library(lists)).

cut(DatasetName) :-
	read_dataset(DatasetName, [HeadList, PrettyGenderList, FactsList]),
	declare([callie, carl, martha, mark, lorrie, leon, steve, susan, rachel, paul, paula, oma, own, m7, m6, m5, m4, m3, f2, m1], People),
	
	relationship_filter(HeadList, People, HeadRes),
	relationship_filter(PrettyGenderList, People, PrettyGenderRes),
	relationship_filter(FactsList, People, FactsRes),
	
	prettify_gender_format(GenderRes, PrettyGenderRes),

	write_new_dataset_on_file(DatasetName, [HeadRes, GenderRes, FactsRes]).


read_dataset(DatasetName, [HeadList, PrettyGenderList, FactList]) :-
	atom_concat(DatasetName, '.dat', DatasetPath),
	consult(DatasetPath),
	example(HeadList, _, [person(GenderList)], facts(FactList)),
	prettify_gender_format(GenderList, PrettyGenderList).


declare(X, X).


prettify_gender_format([], []).

prettify_gender_format([[A,male]|NoPrettyTail], [male(A)|PrettyTail]) :-
	prettify_gender_format(NoPrettyTail, PrettyTail).

prettify_gender_format([[A,female]|NoPrettyTail], [female(A)|PrettyTail]) :-
	prettify_gender_format(NoPrettyTail, PrettyTail).


relationship_filter(All, People, Res) :-
	list_to_ord_set(People, PeopleSet),
	relationship_filter_loop(All, PeopleSet, [], Res).

relationship_filter_loop([], _, Acc, Acc).

relationship_filter_loop([H|T], People, Acc, Res) :-
	H =.. [BinaryRel, A, B],
	list_to_ord_set([A,B], Candidate),
	ord_subset(Candidate, People),
	append(Acc, [H], NewAcc),
	relationship_filter_loop(T, People, NewAcc, Res).

relationship_filter_loop([H|T], People, Acc, Res) :-
	H =.. [UnaryRel, A],
	ord_member(A, People),
	append(Acc, [H], NewAcc),
	relationship_filter_loop(T, People, NewAcc, Res).

relationship_filter_loop([H|T], People, Acc, Res) :-
	relationship_filter_loop(T, People, Acc, Res).


write_new_dataset_on_file(DatasetName, [Heads, Gender, Facts]) :-
	atom_concat(DatasetName, '-subset.dat', DatasetPath),
	open(DatasetPath, write, Stream),
	write(Stream, example(Heads, [], [person(Gender)], facts(Facts))),
	write(Stream, '.'),
	close(Stream).

