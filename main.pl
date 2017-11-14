:- use_module('kb-construction/getData', [generate/3]).
:- use_module(kbReader, [read_kb/3]).
:- use_module('graph-construction/graphBuilder',[build_multi_graph/3]). 
:- use_module('graph-construction/graphPrinter', [print_multi_graph/4]).
:- use_module('graph-fixing/graphFixer', [aspartix/3, get_aspartix_result/2]).
:- use_module('graph-fixing/tunFileConstruction', [create_tun_files/3]).
:- use_module(mainUtilities).
:- use_module(library(readutil)).


/*
  Build a knowledge base starting from a .dat file in the main folder of the project
  You can corrupt the KB with a percentual in range [0->1]
  KB will be placed in "knowledge-base" directory.

  Usage example:
  build_kb(dummyfamily, 0.1)
*/
build_kb(Datasetname, CorruptionPerc) :-
  writeln('Building knowledge base...'),
  generate(Datasetname, CorruptionPerc, OutputFilename),
  write('Done! Filename: '), writeln(OutputFilename).


/*
  Build a graph starting from a KB place in the "knowledge base" directory.
  Graph will be placed in "graph-construction/output" directory.
  You have to specify a constraint list to build the graph.
  (Constraint domain: {and, nand, or, xor, nor, if, iff})

  If the starting KB contains k clause (more than one dataset),
  k graphs will be generated.
    
  The predicate returns the paths of the created graphs for further uses.

  Usage esample:
  build_graph_from_kb('dummyfamily-0.1', [and,or], GraphpathList).
*/
build_graph_from_kb(KbName, ConstraintList, GraphpathList) :-
  writeln('Building graph...'),
  build_graph_multi_constraint(KbName, ConstraintList, Heads, GraphpathList),
  length(GraphpathList, N),
  write('Done! '), nl,
  write('Num of built graphs: '), write(N), nl.

build_graph_multi_constraint(KbName, ConstraintList, Heads, FilenameList) :-
  set_input_path_from_kb(KbName, KbPath),
  read_kb(KbPath, Heads, Bodies),
  build_multi_graph(Bodies, ConstraintList, Graphs),
  atomic_list_concat(ConstraintList, FilterName),
  print_multi_graph(Graphs, KbName, FilterName, FilenameList).


/*
  Use ASPARTIX to find the graph's extensions based of "prefex" and "stable" semantic.
  This predicate needs just one input: the name list of the graphs to process.

  Each input file has to be placed in the directory "graph-construction/output"
  Each output.solved file will be placed in the directory "graph-fixing/output"

  If ASPARTIX finds k extensions for the graph g, in the output folder will be
  generated k file "g_sol1.solved", ...., "g_solk.solved"

  In the end, some quantitative informations will be printed out about the ASPARTIX results.

  Usage example:
  build_solved_file_from_graph(['graph-construction/output/0-dummyfamily-0.1-andor.dl']).
*/
build_solved_file_from_graph(GraphpathList) :-
  get_graphname_list(GraphpathList, GraphnameList),
  writeln('Let ASPARTIX arguing prefex solutions...'),
  aspartix(GraphnameList, prefex, PrefexFixedBodies),
  writeln('Argued!'),
  writeln('Let ASPARTIX arguing stable solutions...'),
  aspartix(GraphnameList, stable, StableFixedBodies),
  writeln('Argued!'),
  evaluate(GraphnameList).


/*
  Generates the input file for INTHELEX system from the solved file built via ASPARTIX
  (INTHELEX page: http://bit.ly/2hps6Ej)

  Required format is file.tun

  If ASPARTIX found k solution for a graph g on a constraint list L and a semantic s,
  k file.tun will be generated and they will be place in the folder "output" in the
  project's folder root.

  Usage example:
  tun_from_solved('dummyfamily-0.1', '0-dummyfamily-0.1-andor-prefex'). 
*/
tun_from_solved(KbName, SolvedFilename) :-
  atomic_list_concat(['knowledge-base/', KbName, '.kb'], KbPath),
  read_kb(KbPath, Heads, _),

  atomic_list_concat([SolvedFilename, '.solved'], SolvedWithExt),
  get_aspartix_result([SolvedWithExt], FixedBodiesWithNeg),
  remove_neg(FixedBodiesWithNeg, FixedBodies),

  create_tun_files([SolvedFilename], Heads, FixedBodies).