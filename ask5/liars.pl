:- lib(ic).

liars(Declared, Liars):-
	length(Declared, N),
	length(BinLiars, N),
	BinLiars #:: [0,1],											% Solution: (Binary) List with Length equal to that of Declared, with values (1 : isLiar, 0 : isNotLiar)
	constraint(Declared, Liars, Sum),
	Sum #= sum(Liars),	
	search(Liars, 0, input_order, indomain, complete, []).		% After setting variables, domains and constraints, use simple search to produce solution


constraint([], [], _).
constraint([MinLiars|Tail], [IsLiar|Tail1], SumLiars):-
	IsLiar #= (MinLiars #> SumLiars),							% Using Constraint as Variable. IsLiar gets value 1, if the minLiars declared are more than the Sum of Liars
	constraint(Tail, Tail1, SumLiars).
