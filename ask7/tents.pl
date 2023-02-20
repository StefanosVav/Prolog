% Using ECLiPSe and the IC library

:- lib(ic).
:- lib(branch_and_bound).

tents(RowTents, ColumnTents, Trees, Tents):-
	length(RowTents, N),
	length(ColumnTents, M),
	FieldLength is N*M,
	length(Field, FieldLength),
	Field #:: 0..1,
	tree_const(Trees, Field, N, M),							% Covering Constraints 1,3: Every tree must have a tent neighboor, There can't be a tent on a tree's position
	no_neighboor_tents_const(Field, Field, N, M, 1),		% Covering Constraint 2: A tent can't have another tent as a neighboor
	tents_per_row_const(RowTents, Field, M, 1),				% Covering Constraint 4 (part1): There's a maximum amount of Tents in each Row
	tents_per_col_const(ColumnTents, Field, N, M, 1),		% Covering Constraint 4 (part2): There's a maximum amount of Tents in each Column
	Cost #= sum(Field), !,
	bb_min(labeling(Field), Cost, bb_options{solutions:all}),	% Covering constraint 5: Place minimum amount of Tents
	produce_tents(Field, M, 1, Tents).
	
% Every tree must have a tent neighboor, There can't be a tent on a tree's position 
tree_const([], _, _, _).
tree_const([ I - J | Rest ], Field, N, M):-
	Pos is (I-1)*M + J,
	element_at(Pos, Field, Val),
	Val #= 0,										% Get the value at tree's position and demand it be equal to 0, aka there can't be a tent on a tree's position
	getUpLeftVal(Pos, Field, N, M, Val1),			
	getUpRightVal(Pos, Field, N, M, Val2),
	getUpVal(Pos, Field, N, M, Val3),
	getLeftVal(Pos, Field, N, M, Val4),
	getRightVal(Pos, Field, N, M, Val5),
	getDownLeftVal(Pos, Field, N, M, Val6),
	getDownRightVal(Pos, Field, N, M, Val7),
	getDownVal(Pos, Field, N, M, Val8),
	Val1 + Val2 + Val3 + Val4 + Val5 + Val6 + Val7 + Val8 #>= 1,		% Get values of all neighboor positions in Field and demand their sum >= 1, aka there's at least 1 tent neighboor to each tree
	tree_const(Rest, Field, N, M).	

% A tent can't have another tent as a neighboor.
no_neighboor_tents_const([_], _, _, _, _).
no_neighboor_tents_const([_|Rest], Field, N, M, Pos):-
	0 is Pos mod M,									% No need to check if we're at the last column of a row
	Pos1 is Pos + 1,
	no_neighboor_tents_const(Rest, Field, N, M, Pos1).
no_neighboor_tents_const([Val|Rest], Field, N, M, Pos):-			
	getRightVal(Pos, Field, N, M, Val1),
	getDownRightVal(Pos, Field, N, M, Val2),
	getDownVal(Pos, Field, N, M, Val3),
	Val + Val1 + Val2 + Val3 #=< 1,					% For each Pos in Field, add its Value to that of PosRight, PosDownRight, PosDown and demand their sum =< 1, aka there can never be a tent neighbooring another tent
	Pos1 is Pos + 1,
	no_neighboor_tents_const(Rest, Field, N, M, Pos1).
	
% There's a maximum amount of Tents in each Row
tents_per_row_const([], _, _, _):- !.
tents_per_row_const([Max|Rest], Field, M, Row):-
	Max >= 0,											% There is a constraint of max tents in each row only when max is a positive number
	getRowValues(Field, M, Row, 1, RowVals),
	sum(RowVals) #=< Max,
	Row1 is Row + 1,
	tents_per_row_const(Rest, Field, M, Row1).
tents_per_row_const([Max|Rest], Field, M, Row):-
	Max < 0,											% If max < 0, there's no constraint for this row
	Row1 is Row + 1,
	tents_per_row_const(Rest, Field, M, Row1).

% Make a list of all values in a row of the field
getRowValues(Field, M, Row, J, [Val]):-
	J == M,
	Pos is (Row-1)*M + J,
	element_at(Pos, Field, Val).
getRowValues(Field, M, Row, J, [Val|Rest]):-
	J < M,
	Pos is (Row-1)*M + J,
	element_at(Pos, Field, Val),
	J1 is J + 1,
	getRowValues(Field, M, Row, J1, Rest).
	
% There's a maximum amount of Tents in each Column
tents_per_col_const([], _, _, _, _).
tents_per_col_const([Max|Rest], Field, N, M, Col):-
	Max >= 0,											% There is a constraint of max tents in each column only when max is a positive number
	getColValues(Field, N, M, Col, 1, ColVals),
	sum(ColVals) #=< Max,
	Col1 is Col + 1,
	tents_per_col_const(Rest, Field, N, M, Col1).
tents_per_col_const([Max|Rest], Field, N, M, Col):-
	Max < 0, 											% If max < 0, there's no constraint for this column
	Col1 is Col + 1,
	tents_per_col_const(Rest, Field, N, M, Col1).
	
% Make a list of all values in a column of the field
getColValues(Field, N, M, Col, I, [Val]):-
	I == N,
	Pos is (I-1)*M + Col,
	element_at(Pos, Field, Val).
getColValues(Field, N, M, Col, I, [Val|Rest]):-
	I < N,
	Pos is (I-1)*M + Col,
	element_at(Pos, Field, Val),
	I1 is I + 1,
	getColValues(Field, N, M, Col, I1, Rest).
	
% Produce coordinates I - J from each position in Field where Val == 1, aka get coordinates of tents
produce_tents([], _, _, []).
produce_tents([1 | Rest], M, Pos, [ I - J | T]):-
	I is (Pos-1) // M + 1,
	J is (Pos-1) mod M + 1,
	Pos1 is Pos + 1,
	produce_tents(Rest, M, Pos1, T).
produce_tents([0 | Rest], M, Pos, Tents):-
	Pos1 is Pos + 1,
	produce_tents(Rest, M, Pos1, Tents).
	
	
% Val = 0 if Pos is in the first Row or the first Column, and thus there's no UpLeft position in the Field. 
getUpLeftVal(Pos, _, _, M, 0):-
	Pos =< M, !.
getUpLeftVal(Pos, _, _, M, 0):-
	1 is Pos mod M, !.
getUpLeftVal(Pos, Field, _, M, Val):-
	PosUpLeft is Pos - M - 1,
	element_at(PosUpLeft, Field, Val).
	
% Val = 0 if Pos is in the first Row or the last Column, and thus there's no UpRight position in the Field. 
getUpRightVal(Pos, _, _, M, 0):-
	Pos =< M, !.
getUpRightVal(Pos, _, _, M, 0):-
	0 is Pos mod M, !.
getUpRightVal(Pos, Field, _, M, Val):-
	PosUpRight is Pos - M + 1,
	element_at(PosUpRight, Field, Val).
	
% Val = 0 if Pos is in the first Row, and thus there's no Up position in the Field. 
getUpVal(Pos, _, _, M, 0):-
	Pos =< M, !.
getUpVal(Pos, Field, _, M, Val):-
	PosUp is Pos - M,
	element_at(PosUp, Field, Val).

% Val = 0 if Pos is in the first Column, and thus there's no Left position in the Field. 
getLeftVal(Pos, _, _, M, 0):-
	1 is Pos mod M, !.
getLeftVal(Pos, Field, _, _, Val):-
	PosLeft is Pos - 1,
	element_at(PosLeft, Field, Val).

% Val = 0 if Pos is in the last Column, and thus there's no Right position in the Field. 
getRightVal(Pos, _, _, M, 0):-
	0 is Pos mod M, !.
getRightVal(Pos, Field, _, _, Val):-
	PosRight is Pos + 1,
	element_at(PosRight, Field, Val).
	
% Val = 0 if Pos is in the last Row or the first Column, and thus there's no DownLeft position in the Field. 
getDownLeftVal(Pos, _, N, M, 0):-
	Pos >= N*M - M + 1, !.
getDownLeftVal(Pos, _, _, M, 0):-
	1 is Pos mod M, !.
getDownLeftVal(Pos, Field, _, M, Val):-
	PosDownLeft is Pos + M - 1,
	element_at(PosDownLeft, Field, Val).
	
% Val = 0 if Pos is in the last Row or the last Column, and thus there's no DownRight position in the Field.
getDownRightVal(Pos, _, N, M, 0):-
	Pos >= N*M - M + 1, !.
getDownRightVal(Pos, _, _, M, 0):-
	0 is Pos mod M, !.
getDownRightVal(Pos, Field, _, M, Val):-
	PosDownRight is Pos + M + 1,
	element_at(PosDownRight, Field, Val).
	
% Val = 0 if Pos is in the last Row, and thus there's no Down position in the Field.
getDownVal(Pos, _, N, M, 0):-
	Pos >= N*M - M + 1, !.
getDownVal(Pos, Field, _, M, Val):-
	PosDown is Pos + M,
	element_at(PosDown, Field, Val).
	
element_at(1, [X|_], X).
element_at(K, [_|L], X) :- 
	element_at(K1, L, X), K is K1 + 1.
	
	