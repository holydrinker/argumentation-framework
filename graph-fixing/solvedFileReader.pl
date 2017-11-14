:- module(solvedFileReader, [get_lines_from_solved_file/2]).

:- use_module(library(readutil)).

/*
	It reads the graph.solved file and returns the extension set in a Prolog-list format
*/
get_lines_from_solved_file(Filename, Result) :-
	atomic_list_concat(['graph-fixing/output/',Filename], FilePath),
	open(FilePath, read, Stream),
	skip_first_line(Stream),
	process_stream(Stream, Lines),
	remove_empty_line(Lines, Result).


skip_first_line(Stream) :-
	read_line_to_codes(Stream, Codes).


process_stream(Stream, []) :-
	read_line_to_codes(Stream, Codes),
	no_rules_found(Codes),
	atom_codes(L, Codes),
	process_stream(Stream, Lines).

process_stream(Stream, [L|Lines]) :-
	read_line_to_codes(Stream, Codes),
	Codes \= end_of_file,
	atom_codes(L, Codes),
	process_stream(Stream, Lines).

process_stream(Stream, []) :-
	close(Stream). 

no_rules_found([123,125]).

	
remove_empty_line([], []).

remove_empty_line([''|T], T1) :-
	remove_empty_line(T, T1).

remove_empty_line([H|T], [H|T1]) :-
	remove_empty_line(T, T1).