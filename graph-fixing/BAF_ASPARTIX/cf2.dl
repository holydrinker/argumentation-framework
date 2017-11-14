%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% INPUT AF (A,R) given via arg(.) and att(.,.).
%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% HELPERS --- as usual
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
lt(X,Y) :- arg(X),arg(Y), X<Y.
nsucc(X,Z) :- lt(X,Y), lt(Y,Z).
succ(X,Y) :- lt(X,Y), not nsucc(X,Y).
ninf(X) :- lt(Y,X).
nsup(X) :- lt(X,Y).
inf(X) :- not ninf(X), arg(X).
sup(X) :- not nsup(X), arg(X).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Guess conflict-free S\subseteq A
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

in(X)  :- not out(X), arg(X).
out(X) :- not in(X),  arg(X).
:- in(X), in(Y), att(X,Y).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copy all arguments to set with least index.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

arg_set(N,X) :- arg(X), inf(N).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% reachability and component defeated args wrt index N
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

reach(N,X,Y):-arg_set(N,X),arg_set(N,Y),att(X,Y).
reach(N,X,Y):-arg_set(N,X),att(X,Z),reach(N,Z,Y).
d(N,X) :- arg_set(N,Y), arg_set(N,X), in(Y), att(Y,X), not reach(N,X,Y).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% copy remaining arguments to next index.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

arg_set(M,X) :- arg_set(N,X), succ(N,M), not d(N,X).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% F_S is obtained by using the arg_set with the largest index M 
% (i.e. A \setminus \Delta, where \Delta is given by predicates d(M,.)
% and restrict to components.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

arg_new(X) :- sup(M), arg_set(M,X).
att_new(X,Y) :- arg_new(X), arg_new(Y), att(X,Y), reach(M,Y,X),  sup(M). 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% We check whether S is maximal conflict-free for F_S
%
% S is maximal conflict-free, 
% if there is no x \not\in S such that x is not conflicting
%
% an argument X \not\in S is conflicting, if: 
% (i) it is attacked by an argument Y \in S, or
% (ii) it attacks an argument Y \in S, or
% (iii) it is self-attacking
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

conflicting(X) :- att_new(Y,X), out(X), in(Y).
conflicting(X) :- att_new(X,Y), out(X), in(Y).
conflicting(X) :- att_new(X,X).
:- not conflicting(X), out(X), arg_new(X).

