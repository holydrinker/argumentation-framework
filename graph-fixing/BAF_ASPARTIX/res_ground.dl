
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Verification if guessed set in(X) is in res_gr(F)
% input: AF F=(A,R)
% guess all conflict free sets U of F
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
in(X) :- not out(X), arg(X).
out(X) :- not in(X), arg(X).

:- in(X), in(Y), att(X,Y).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% HELPERS --- as usual
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
lt(X,Y) :- arg(X),arg(Y), X<Y.
nsucc(X,Z) :- lt(X,Y), lt(Y,Z).
succ(X,Y) :- lt(X,Y), not nsucc(X,Y).
ninf(X) :- lt(Y,X).
nsup(X) :- lt(X,Y).
inf(X) :- not ninf(X), arg(X).
sup(X) :- not nsup(X), arg(X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copy all arguments to set with least index.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
arg_set(N,X) :- arg(X), inf(N).
inU(N,X) :- in(X), inf(N).
defeatN(N,Y,X) :- arg_set(N,X), arg_set(N,Y), att(Y,X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% compute the grounded extension of the AF with index N
% inS(N,X)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
defended_upto(N,X,Y) :- inf(Y), arg_set(N,X), not defeatN(N,Y,X).
defended_upto(N,X,Y) :- inf(Y), inS(N,Z), defeatN(N,Z,Y), defeatN(N,Y,X).
defended_upto(N,X,Y) :- succ(Z,Y), defended_upto(N,X,Z), not defeatN(N,Y,X).
defended_upto(N,X,Y) :- succ(Z,Y), defended_upto(N,X,Z), inS(N,V), defeatN(N,V,Y), defeatN(N,Y,X).

defended(N,X) :- sup(Y), defended_upto(N,X,Y).
inS(N,X) :- defended(N,X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% compute alpha(N,X) 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
alpha(N,X) :- inS(N,X).
alpha(N,X) :- inS(N,Y), defeatN(N,Y,X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% inU \cap alpha
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
uAlpha(N,X) :- inU(N,X), alpha(N,X).

:- uAlpha(N,X), not inS(N,X).
:- not uAlpha(N,X), inS(N,X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% compute MR(cut(N,F))
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% compute cut(F)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
cut(N,X) :- arg_set(N,X), not alpha(N,X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% check the 3 conditions of Lemma 6 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% SCC
reach(N,X,Y) :- cut(N,X), cut(N,Y), defeatN(N,X,Y).
reach(N,X,Y) :- cut(N,X), defeatN(N,X,Z), reach(N,Z,Y), X!=Y.

%% condition a) self attacks
n_mr(N,X) :- cut(N,X), defeatN(N,X,X). 

%% condition b) symmetric
nsym(N,X) :- cut(N,X), cut(N,Y), defeatN(N,X,Y), not defeatN(N,Y,X), reach(N,X,Y), reach(N,Y,X), X!=Y. 
nsym(N,Y) :- cut(N,X), cut(N,Y), defeatN(N,X,Y), not defeatN(N,Y,X), reach(N,X,Y), reach(N,Y,X), X!=Y. 

%% condition c) undirected graph acyclic
reachnotvia(N,X,V,Y) :- defeatN(N,X,Y), cut(N,V), reach(N,X,Y), reach(N,Y,X), X!=V,Y!=V.
reachnotvia(N,X,V,Y) :- reachnotvia(N,X,V,Z), reachnotvia(N,Z,V,Y), reach(N,X,Y), reach(N,Y,X), Z!=V, X!=V,Y!=V.

cyc(N,X,Y,Z) :- defeatN(N,X,Y), defeatN(N,Y,X), defeatN(N,Y,Z), defeatN(N,Z,Y), 
	     reachnotvia(N,X,Y,Z), X!=Y, Y!=Z, X!=Z. 

bad(N,Y):- cyc(N,X,U,V), reach(N,X,Y), reach(N,Y,X).
bad(N,Y) :- n_mr(N,X), reach(N,X,Y), reach(N,Y,X).
bad(N,Y) :- nsym(N,X), reach(N,X,Y), reach(N,Y,X).
pos_mr(N,X) :- cut(N,X), not bad(N,X), not n_mr(N,X), not nsym(N,X).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% minimal relevant SCC: mr
% possible mr pos_mr(N,X) fulfills all conditions of Lemma 6
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
notminimal(N,Z):- reach(N,X,Y), reach(N,Y,X), reach(N,X,Z), not reach(N,Z,X).
mr(N,X) :- pos_mr(N,X), not notminimal(N,X).

t(N,X) :- inU(N,X), not inS(N,X).
nemptyT(N) :- t(N,X).
emptyT(N) :- not nemptyT(N), arg_set(N,X).

exitsMR(N) :- mr(N,X), cut(N,X).
nmr(N) :- not exitsMR(N), cut(N,X).

true(N) :- emptyT(N), not exitsMR(N).
:- nmr(N), nemptyT(N).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% check for stable: if T is stable in W, => t is stable in mr
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
defeated(N,X) :- mr(N,X), mr(N,Y), t(N,Y), defeatN(N,Y,X).
not_defeated(N,X) :- mr(N,X), not defeated(N,X).
:- not t(N,X), not_defeated(N,X), mr(N,X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% (T \cap W)+
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
tw_plus(N,Y) :- t(N,X), mr(N,X), defeatN(N,X,Y).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% next iteration
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
arg_set(M,X) :- cut(N,X), not mr(N,X), not tw_plus(N,X), succ(N,M), not true(N).

inU(M,X) :- t(N,X), not mr(N,X), succ(N,M), not true(N).

