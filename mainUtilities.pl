:- module(mainUtilities, 
	[set_input_path_from_kb/2, 
	 get_graphname_list/2,
   evaluate/1,
   remove_neg/2]).

set_input_path_from_kb(KbName, KbPath) :-
  atom_concat(KbName, '.kb', _KbName),
  atom_concat('knowledge-base/', _KbName, KbPath).

/*
  Extracts a list of graph from a list of graph path
*/
get_graphname_list([], []).
get_graphname_list([P|Paths], [N|Names]) :-
  atom_concat('graph-construction/output/', N_Ext, P),
  atom_concat(N, '.dl', N_Ext),
  get_graphname_list(Paths, Names).


/*
  Compute metrics on a graph lists
*/
evaluate(GraphnameList) :-
  count_arg_att_supp(GraphnameList),
  count_solutions(GraphnameList, prefex, NumOfPrefex),
  count_solutions(GraphnameList, stable, NumOfStable),
  write('prefex solution: '),
  writeln(NumOfPrefex),
  write('stable solution: '),
  writeln(NumOfStable).  

count_arg_att_supp([Graphname]) :-
  atomic_list_concat(['graph-construction/output/', Graphname, '.dl'], Graphfile),
  consult(Graphfile),
  
  findall(X, arg(X), Arg),
  length(Arg, ArgN),
  write('arguments: '),
  writeln(ArgN),

  findall([X,Y], att(X,Y), Att),
  length(Att, AttN),
  write('attacks: '),
  writeln(AttN),

  findall([X,Y], support(X,Y), Sup),
  length(Sup, SupN),
  write('support: '),
  writeln(SupN).

count_solutions([Filename], Semantic, NumOfLines) :-
  atomic_list_concat(['graph-fixing/output/', Filename, '-', Semantic, '.solved'], Filepath),
  open(Filepath, read, Stream),
  count_line(Stream, 0, NumOfLines).

count_line(Stream, N, NumOfLines) :-
  read_line_to_codes(Stream, Codes),
  Codes \= end_of_file,
  N1 is N + 1,
  count_line(Stream, N1, NumOfLines).

count_line(Stream, N, NumOfLines) :-
  RemoveFirst is N - 1,
  NumOfLines is RemoveFirst / 2,
  close(Stream).


/*
  Given a list of positive and negative atoms, it filter the positive ones.
  - positive atom: p(a)
  - negativa atom: neg(p(a))
*/
remove_neg([], []).

remove_neg([FirstClauseExtSet|T], [FixedSet|NewT]) :-
  remove_neg_from_extension_set(FirstClauseExtSet, FixedSet),
  remove_neg(T, NewT).

remove_neg_from_extension_set([], []).

remove_neg_from_extension_set([FirstExt|T], [NewExt|NewT]) :-
  remove_neg_from_single_list(FirstExt, NewExt),
  remove_neg_from_extension_set(T, NewT).

remove_neg_from_single_list([], []).

remove_neg_from_single_list([neg(H)|T], NewT) :-
  remove_neg_from_single_list(T, NewT), !.

remove_neg_from_single_list([H|T], [H|NewT]) :-
  remove_neg_from_single_list(T, NewT), !.