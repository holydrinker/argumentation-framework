:- module(kbReader, [read_kb/3]).

:- use_module(library(readutil)).
:- use_module(library(charsio)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).

read_kb(KBpath, Heads, Bodies) :-
  read_file_to_terms(KBpath, ClauseList, []),
  get_heads_and_bodies(ClauseList, [[], []], [Heads, Bodies]).


get_heads_and_bodies([], [Heads, Bodies], [Heads, Bodies]).

get_heads_and_bodies([H|T], [HeadAcc, BodyAcc], [Heads, Bodies]) :-
  get_head_and_body_from_single_clause(H, HeadList, BodyList),
  append(HeadAcc, [HeadList], NewHeadAcc),
  append(BodyAcc, [BodyList], NewBodyAcc),
  get_heads_and_bodies(T, [NewHeadAcc, NewBodyAcc], [Heads, Bodies]).

/*
  Works on clause in this format:

  [Head_1, ..., Head_n] :-
    Body_1,
    ...,
    Body_m.

  P.S.
  [58,45] is :-
  91 is [
  93 is ]
*/
get_head_and_body_from_single_clause(Clause, HeadList, BodyList) :-
  term_to_atom(Clause, AtomClause),
  atom_codes(AtomClause, ClauseCode),
  split_head_and_body(ClauseCode, [], HeadList, BodyList).

split_head_and_body([58,45|T], HeadAcc, HeadList, BodyList) :-
  codes_to_list(HeadAcc, HeadList),
  append([91], T, B1),
  append(B1, [93], B2),
  codes_to_list(B2, BodyList).

split_head_and_body([H|T], HeadAcc, HeadList, BodyList) :-
  append(HeadAcc, [H], NewHeadAcc),
  split_head_and_body(T, NewHeadAcc, HeadList, BodyList).

/*
  Convert a list of codes like this [1,2,3,4] into a prolog list
  like this [1234] based on ASCII table description http://www.asciitable.com/
*/
codes_to_list(Codes, Res) :-
  atom_codes(AtomCodes, Codes),
  term_to_atom(Res, AtomCodes).
