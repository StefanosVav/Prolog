:- lib(ic).
:- lib(ic_global).

carseq(Sequence) :-
    C is classes,
	Opt is options,
	sum(C, TotalCars),
    length(Sequence, TotalCars),							% Solution: List Sequence with Length = TotalCars, and values (0-6 for each configuration)
	length(C, NumConfigs),
    Sequence #:: 1..NumConfigs,
    occ_const(1, Sequence, C),
    cap_const(C, Opt, Sequence),
	search(Sequence, 0, input_order, indomain, complete, []).

% Each configuration (1..NumConfigs) can exist in the Sequence List as many times as it exists in List classes
occ_const(_, _, []).
occ_const(Conf, Seq, [Occ|Rest]):-
    occurrences(Conf, Seq, Occ),
    ConfNext is Conf + 1,
    occ_const(ConfNext, Seq, Rest).
	
% There can only be K cars that demand an option every M cars in a row. Using sequence_total/7 from lib(ic_global)
cap_const(_, [], _).
cap_const(C, [M/K/O | Rest], Seq):-
    sumCarsOpt(C, O, SumCars),								% Returns the sum of cars that require a certain option
	getPosOne(O, 1, PosList),
    sequence_total(SumCars, SumCars, 0, K, M, Seq, PosList),
    cap_const(C, Rest, Seq).

% Add cars in index Num from List classes, if value of same index in List O of List Options is 1
sumCarsOpt([], [], 0).
sumCarsOpt([Num|Rest], [1|Tail], Sum):-
	sumCarsOpt(Rest, Tail, SumRest),
	Sum is Num + SumRest.
sumCarsOpt([_|Rest], [0|Tail], Sum):-
	sumCarsOpt(Rest, Tail, Sum).
	
% Get positions of List O of List Options where value is 1
getPosOne([], _, []).
getPosOne([1 | Rest], Ind, Pos):-
	Ind1 is Ind + 1,
	getPosOne(Rest, Ind1, RestPos),
	append([Ind], RestPos, Pos).
getPosOne([0 | Rest], Ind, Pos):-
	Ind1 is Ind + 1,
	getPosOne(Rest, Ind1, Pos).