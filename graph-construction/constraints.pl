/* 
	AND
	Sono gli atomi che obbligatoriamente devono convivere nella kb
*/
ic(and([married(A,B), female(A)])) :-
	married(A,B),
	female(A).

ic(and([married(A,B), male(B)])) :-
	married(A,B),
	male(B).

/*
	NAND
	Questo operatore logico restituisce F solo quando due atomi sono T.
  	Quindi ogni espressione logica deve contenere almeno un atomo falso 
  	(dove per falso si intende non presente nel corpo).
 */
ic(nand([married(A,B), male(A)])) :-
	married(A,B),
	male(A).

ic(nand([married(A,B), female(B)])) :-
	married(A,B),
	female(B).

ic(nand([parent(A,B), married(A,B)])) :-
	parent(A,B),
	married(A,B).

ic(nand([parent(A,B), married(B,A)])) :-
	parent(A,B),
	married(B,A).

ic(nand([married(A,B), married(B,A)])) :-
	married(A,B),
	married(B,A).

ic(nand([parent(A,B), parent(B,A)])) :-
	parent(A,B),
	parent(B,A).

ic(nand([male(A), female(A)])) :-
	male(A),
	female(A).

/*
 	IF
 	Dati due atomi, l'operatore IF è verificato se il primo implica il secondo.
 */
ic(if([married(A,B), female(A)])) :-
	married(A,B),
	female(A).

ic(if([married(A,B), male(B)])) :-
	married(A,B),
	male(B).

ic(if([parent(A,X), married(A,Y)])) :-
	parent(A,X),
	married(A,Y).

ic(if([parent(A,X), married(Y,A)])) :-
	parent(A, X),
	married(Y, A).


/* 
	IFF
	Dati due atomi, l'operatore IFF è verificato se il primo implica il secondo e viceversa
*/

/*
	Il vincolo che proviamo a modellare è il seguente:
	parent(A,B) IFF parent(C,B), married(A,C)

	essendo il secondo atomo un congiunzione di due atomi, abbiamo provato a dividerlo come segue:
*/

/*
parent(A,B) IFF parent(C,B)
a condizione che married(A,C)
*/
ic(iff([parent(A,B), parent(C,B)])) :-
	parent(A,B),
	parent(C,B),
	married(A,C).

/*
parent(A,B) IFF married(A,C)
a condizione che parent(C,B)
*/
ic(iff([parent(A,B), married(A,C)])) :-
	parent(A,B),
	married(A,C),
	parent(C,B).

/*
parent(A,B) IFF married(C,A)
a condizione che parent(C,B)
*/
ic(iff([parent(A,B), married(C,A)])) :-
	parent(A,B),
	married(C,A),
	parent(C,B).


/*
	NOR 
	Restituisce True quando entrambi gli elementi sono False.
  	Bisogna esprimere coppie di atomi che non possono trovarsi insieme nel corpo,
  	ovvero se non c'è una non ci deve essere nemmeno l'altro
*/
ic(nor([married(A,X), parent(A,Y)])) :-
	married(A,X),
	parent(A,Y).


/*
	OR
	Dati due atomi, l'operatore OR restituisce True se almeno uno dei due è True.
	Bisogna esprimere coppie di atomi in cui se uno è presente nella kb, non ha bisogno della presenza dell'altro.
*/
ic(or([parent(A,X), parent(Y,A)])) :-
	parent(A,X),
	parent(Y,A).

/*
ic(or([parent(A,X), male(A)])) :-
	parent(A,X),
	male(A).

ic(or([parent(A,X), female(A)])) :-
	parent(A,X),
	female(A).

ic(or([parent(X,A), female(A)])) :-
	parent(X,A),
	female(A).

ic(or([parent(X,A), male(A)])) :-
	parent(X,A),
	male(A).
*/

/*
	XOR
	Solo uno dei due può essere presente nella KB

*/
ic(xor([married(A,B), parent(A,B)])) :-
	married(A,B),
	parent(A,B).

ic(xor([married(A,B), married(B,A)])) :-
	married(A,B),
	married(B,A).

ic(xor([parent(A,B), parent(B,A)])) :-
	parent(A,B),
	parent(B,A).

ic(xor([married(A,K), male(A)])) :-
	married(A,K),
	male(A).

ic(xor([married(K,B), female(B)])) :-
	married(K,B),
	female(B).

ic(xor([male(A), female(A)])) :-
	male(A),
	female(A).