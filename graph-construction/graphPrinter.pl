:- module(graphPrinter, [print_multi_graph/4]).

:- use_module(library(lists)).

print_multi_graph([H|T], FileName, Constraint, FilenameList) :-
  pmg_loop([H|T], FileName, 0, Constraint, FilenameList).


/*
  This predicate automatically understand the number of clause in the KB file.
  If there are 3 clause, it understands that 3 graphs have to be created.
  This predicate simply create these graphs and returns the names it chose for them (in a list format)
*/
pmg_loop([], FileName, Idx, Constraint, []).

pmg_loop([H|T], FileName, Idx, Constraint, [CompleteName|OtherNames]) :-
  print_single_graph(H, FileName, Idx, Constraint, CompleteName),
  Idx1 is Idx + 1,
  pmg_loop(T, FileName, Idx1, Constraint, OtherNames).

/*
  This methods cope with the problem of print a single graph in the root directory of this project.
  This method doesn't know how many graphs have to be printed, so it wants an integer as input.
  In this way, if the calling predicate (pmg_loop) call it with an autoinc integer in the args list,
  every graph obtained from the same KB will be printed with a different name (kb1.baf, kb2.baf, ecc...).

  Definitely, it print the graph and return the auto-built path of the output file.
*/
print_single_graph(_G, Filename, Idx, Constraint, CompletePath) :-
  flatten(_G, G),
  set_output_path(Filename, Constraint, Idx, CompletePath),
  open(CompletePath, write, Stream),
  psg_loop(G, Stream).

set_output_path(Filename, Constraint, _Idx, CompletePath) :-
  atom_number(Idx, _Idx),
  atomic_list_concat(
    ['graph-construction/output/', Idx, '-', Filename, '-', Constraint, '.dl'],
    CompletePath).


psg_loop([], Stream) :-
  close(Stream).

psg_loop([H|T], Stream) :-
  write(Stream, H), write(Stream, '.'), nl(Stream),
  psg_loop(T, Stream).