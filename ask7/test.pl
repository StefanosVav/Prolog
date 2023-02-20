:- lib(ic).
:- lib(branch_and_bound).

tents(RowTents, ColumnTents, Trees, Tents):-
	length(RowTents, N),
	length(ColumnTents, M),
	FieldLength is N*M,
	length(Field, FieldLength),
	Field #:: 0..1,		
	tree_const(Trees, Field, N, M),
	no_neighboor_tents_const(Field, Field, N, M, 1),
	Cost #= sum(Field), !,
	bb_min(labeling(Field), Cost, bb_options{solutions:all}),	% Covering constraint 5: Place minimum amount of Tents
	produce_tents(Field, M, 1, Tents).
	
produce_tents([], _, _, []).
produce_tents([1 | Rest], M, Pos, [I-J | T]):-
	I is (Pos-1) // M + 1,
	J is (Pos-1) mod M + 1,
	Pos1 is Pos + 1,
	produce_tents(Rest, M, Pos1, T).
produce_tents([0 | Rest], M, Pos, Tents):-
	Pos1 is Pos + 1,
	produce_tents(Rest, M, Pos1, Tents).
	
element_at(1, [X|_], X).
element_at(K, [_|L], X) :- 
	element_at(K1, L, X), K is K1 + 1.
	
	