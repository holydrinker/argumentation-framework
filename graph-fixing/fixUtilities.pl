:- module(fixUtilities, [get_avg_length/2]).

:- use_module(library(lists)).

get_avg_length(ListOfList, AvgLength) :-
	get_length_list(ListOfList, ListOfLength),
	compute_average(ListOfLength, AvgLength).

compute_average(ListOfInt, Avg) :-
	compute_average_loop(ListOfInt, [0, 0], Avg).

compute_average_loop([], [0, 0], 0).

compute_average_loop([], [Sum, N], Res) :-
	Res is div(Sum,N).

compute_average_loop([H|T], [Sum, N], Res) :-
	Sum1 is Sum + H,
	N1 is N + 1,
	compute_average_loop(T, [Sum1, N1], Res).

get_length_list(ListOfList, ListOfLength) :-
	get_length_list_loop(ListOfList, [], ListOfLength).

get_length_list_loop([], Acc, Acc).

get_length_list_loop([H|T], Acc, Res) :-
	is_list(H),
	length(H, N),
	append(Acc, [N], NewAcc),
	get_length_list_loop(T, NewAcc, Res).