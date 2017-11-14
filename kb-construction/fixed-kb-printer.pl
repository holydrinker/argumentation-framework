print_fixed_kbs(OriginalKbName, [HeadsList, BodiesList]) :-
  consult('kb-construction/get-data.pl'),
  atom_concat('knowledge-base/', OriginalKbName, OriginalPath),
  atom_concat(OriginalPath, '-fixed.kb', CompleteName),
  open(CompleteName, write, Stream),
  print_fixed_kbs_loop(CompleteName, [HeadsList, BodiesList], Stream).


print_fixed_kbs_loop(CompleteName, [[],[]], Stream) :-
  close(Stream).

print_fixed_kbs_loop(CompleteName, [[H|Heads], [FB|FixedBodies]], Stream) :-
  print_single_kb(H, FB, Stream),
  open(CompleteName, append, NewStream),
  print_fixed_kbs_loop(CompleteName, [Heads, FixedBodies], NewStream).


print_single_kb(Head, Body, Stream) :-
  nl(Stream),
  nl(Stream),
  create_head(Head, [], Heads),
  set_heads(Heads, Stream),
  create_body(Body, Stream).