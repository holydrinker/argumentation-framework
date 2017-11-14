:- module(graphFixer, [aspartix/3, get_aspartix_result/2]).

:- use_module(solvedFileReader, [get_lines_from_solved_file/2]).
:- use_module(fixUtilities, [get_avg_length/2]).
:- use_module(library(readutil)).
:- use_module(library(charsio)).
:- use_module(library(lists)).
:- use_module(library(lineutils)).

/*
	It reads [graph1.dl, graph2.dl] file list and generate a graph.solved result file for each one.
	Semantic = prefex
*/
aspartix(Graphnames, Semantic, Result) :-
	unix(shell('chmod +x graph-fixing/BAF_ASPARTIX/dlv.bin')),
	run_aspartix(Graphnames, Semantic, WrittenFilenames),
	get_aspartix_result(WrittenFilenames, Result).

run_aspartix([], _, []).

run_aspartix([G|Graphnames], Semantic, [WrittenFileName|Names]) :-
	run_aspartix_loop(G, Semantic, WrittenFileName),
	run_aspartix(Graphnames, Semantic, Names).
	
run_aspartix_loop(Filename, Semantic, WrittenFileName) :- 
	create_command(Filename, Semantic, WrittenFileName, Command),
	unix(shell(Command)).

create_command(Filename, Semantic, WrittenFileName, Command) :-
	atomic_list_concat([Filename, '-', Semantic, '.solved'], WrittenFileName),

	atomic_list_concat(
		[	'graph-fixing/BAF_ASPARTIX/dlv.bin ',
			'graph-fixing/BAF_ASPARTIX/baf.dl ', 
			'-n=50 ',
			'graph-fixing/BAF_ASPARTIX/', Semantic, '.dl ', 
			'graph-construction/output/', Filename, '.dl ',
			'-filter=in > graph-fixing/output/', WrittenFileName], 
		Command).


/*
	A filename list is given.
	For every file, this predicate extract the result from it.
*/
get_aspartix_result([], []).

get_aspartix_result([F|Files], [PartialRes|Result]) :-
	writeln(F),
	get_extensions_from_file(F, PartialRes),
	get_avg_length(PartialRes, AvgLength),
	write('solution avg length: '),
	writeln(AvgLength),
	get_aspartix_result(Files, Result).


/*
	It reads file.solved and returns the list of the preferred extensions
*/
get_extensions_from_file(Filename, Result) :-
	get_lines_from_solved_file(Filename, ExtensionsSet),
	process_all_sets(ExtensionsSet, Result).

process_all_sets([], []).

process_all_sets([Set|Sets], [ProcSet|ProcSets]) :-
	process_extension_set(Set, ProcSet),
	process_all_sets(Sets, ProcSets).

process_extension_set(Set,ProcFixedRules) :-
	atomic_list_concat(Rules, ' ', Set),
	remove_braces(Rules, FixedRules),
	process_rules(FixedRules, ProcFixedRules).

remove_braces([{}], []).

remove_braces([SingleRule], [Res]) :-
	atom_codes(SingleRule, [H|T]),
	reverse(T, [Last|T1]),
	reverse(T1, NewCodes),
	atom_codes(Res, NewCodes).

remove_braces([R|RT], [First|FixedT]) :-
	atom_concat('{', First, R),
	reverse(RT, [Last|Body]),
	atom_concat(FixedLast, '}', Last),
	reverse([FixedLast|Body], FixedT).

process_rules([], []).

process_rules([R|RT], [ProcessedR|ProcessedRT]) :-
	remove_comma(R, RuleWithoutComma),
	atom_to_term(RuleWithoutComma, in(ProcessedR), []),
	process_rules(RT, ProcessedRT).

remove_comma(Rule, RuleWithoutComma) :-
	atom_concat(RuleWithoutComma, ',', Rule).

remove_comma(RuleWithoutComma, RuleWithoutComma).