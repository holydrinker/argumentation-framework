:- module('graph-fixing/tunFileConstruction', [create_tun_files/3]).

:- use_module('../kb-construction/getData', [set_heads/2, create_body/2]).

create_tun_files([], [], []).

create_tun_files([GraphName|RemainingGraphNames], [Heads|RemainingHeads], [FixedBodies|RemainingFixedBodies]) :-
	create_single_tun_file(GraphName, 0, Heads, FixedBodies),
	create_tun_files(RemainingGraphNames, RemainingHeads, RemainingFixedBodies).

create_single_tun_file(GraphName, Idx, Heads, []).

create_single_tun_file(GraphName, Idx, Heads, [H|T]) :-
	set_name(GraphName, Idx, CompleteName),
	open(CompleteName, write, Stream),
	set_heads(Heads, Stream),
	create_body(H, Stream),
	Idx1 is Idx + 1,
	create_single_tun_file(GraphName, Idx1, Heads, T).

set_name(GraphName, Idx, CompleteName) :-	
	atom_number(_Idx, Idx),
	atom_concat('output/', GraphName, ConcatenatedName),
	atom_concat(ConcatenatedName, '_sol', PartialName),
	atom_concat(PartialName, _Idx, IntermediateName),
	atom_concat(IntermediateName, '.tun', CompleteName).