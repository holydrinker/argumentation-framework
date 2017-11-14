:- module(getData, [generate/3, set_heads/2, create_body/2]).

:- use_module(library(lists)).
:- consult('corruption.pl').

generate(DatasetName, AtomContaminationPerc, OutputFilename) :-
  atomic_list_concat([DatasetName, '-', AtomContaminationPerc], OutputFilename),
  atom_concat(DatasetName, '.dat', DatasetPath),
  consult(DatasetPath),
  example(Positive, _, [person(PersonListUgly)], facts(FactsList)),
  format_person_list(PersonListUgly, PersonList, NamesList),
  append(PersonList, FactsList, TruthList),
  config_output_file(OutputFilename, Stream),
  create_head(Positive, [], Heads),
  set_heads(Heads, Stream),
  atom_list_contamination(TruthList, NamesList, AtomContaminationPerc, ResList),
  create_body(ResList, Stream).

format_person_list(PersonList, PersonRes, Names) :-
  format_person_list_loop(PersonList, [], PersonRes, [], Names).

format_person_list_loop([], Acc, Acc, Name, Name).

format_person_list_loop([[A,B]|T], Acc, Res, Name, FinName) :-
  atom_concat(B, '(', _1),
  atom_concat(_1, A, _2),
  atom_concat(_2, ')', Person),
  term_to_atom(NewPerson, Person),
  append(Acc, [NewPerson], NewAcc),
  append(Name, [A], NewName),
  format_person_list_loop(T, NewAcc, Res, NewName, FinName).

config_output_file(DatasetName, Stream) :-
  atom_concat('knowledge-base/', DatasetName, _1),
  atom_concat(_1, '.kb', OutputPath),
  open(OutputPath, write, Stream).

create_head([], HeadAcc, HeadAcc).

create_head([H|T], HeadAcc, Heads) :-
  append(HeadAcc, [H], NewHeadAcc),
  create_head(T, NewHeadAcc, Heads).

set_heads(Heads, Stream) :-
  write(Stream, Heads),  tab(Stream, 4),  write(Stream, ':-'),
  nl(Stream).

create_body([H], Stream) :-
  tab(Stream, 4), write(Stream, H), write(Stream, '.'),
  close(Stream).

create_body([H|T], Stream) :-
  tab(Stream, 4), write(Stream, H), write(Stream, ','),
  nl(Stream),
  create_body(T, Stream).
