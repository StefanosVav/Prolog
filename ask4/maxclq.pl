% Using ECLiPSe and the IC library

:- lib(ic).
:- lib(branch_and_bound).


maxclq(N, D, Clique, Size):-
	create_graph(N, D, G),
	write("G = "), write(G), write("\n"),
	length(BiNodes, N),
	BiNodes #:: 0..1,															% make a List of Length = NumNodes with values (1 : Node in Clique, 0 : Node not in Clique)
	constraints(BiNodes, 1, G),													% Set values based on constraints
	Cost #= N - sum(BiNodes),													% Set Cost as number of Nodes NOT in the Clique
	bb_min(labeling(BiNodes), Cost, bb_options{strategy:restart}),				% Find maximum Clique by minimizing the Cost (= Total_Nodes - Sum_Of_Nodes_In_Clique)
	clq_from_nodes(BiNodes, 1, Clique),											% Produce Clique from Binary Nodes List
	length(Clique, Size),
	!.

% Constraint with the logic described in class explained in line 34
constraints([], _, _).	
constraints([BiNode|Tail], I, Graph):- 
	J is I + 1,
	check_const(BiNode, Tail, I, J, Graph),							% Check constraint for all pairs of each node (Considering each node with I < J in order to not check nodes twice)
	constraints(Tail, J, Graph).

check_const(_, [], _, _, _).
check_const(BiNode, [_|Tail], I, J, Graph):-
	(member(I - J, Graph); member(J - I, Graph)),
	J1 is J + 1,
	check_const(BiNode, Tail, I, J1, Graph).
check_const(BiNode, [BiNode1|Tail], I, J, Graph):- 
	\+ member(I - J, Graph),
	\+ member(J - I, Graph),
	BiNode + BiNode1 #=< 1,											% If Node I and Node J are not connected, their sum in the BiNodes list must be less than 1 (meaning at least 1 of them is not in the Clique)
	J1 is J + 1,
	check_const(BiNode, Tail, I, J1, Graph).


% If Node I has value 1 in BinaryNodes List, add it to the Clique
clq_from_nodes([], _, []).	
clq_from_nodes([1|Tail], I, Clique):-
	I1 is I + 1,
	clq_from_nodes(Tail, I1, RestClique),
	append([I], RestClique, Clique).
clq_from_nodes([0|Tail], I, Clique):-
	I1 is I + 1, 
	clq_from_nodes(Tail, I1, Clique).
	


	



