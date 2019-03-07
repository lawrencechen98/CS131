% An implementation of transpose for lists of lists from an old version of SWI-Prolog's CLPFD Module
% Source: https://stackoverflow.com/questions/4280986/how-to-transpose-a-matrix-in-prolog
transpose([], []).
transpose([F|Fs], Ts) :-
    transpose(F, [F|Fs], Ts).

transpose([], _, []).
transpose([_|Rs], Ms, [Ts|Tss]) :-
        lists_firsts_rests(Ms, Ts, Ms1),
        transpose(Rs, Ms1, Tss).

lists_firsts_rests([], [], []).
lists_firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
        lists_firsts_rests(Rest, Fs, Oss).


% Helper predicates for solving tower puzzle

% Helper predicate to check all rows of a matrix is of length N
rows_length([], _).
rows_length([H|T], N) :- length(H, N), rows_length(T, N).

% Helper predicate for fd_domain to allow use of maplist and give only upper bound
list_range(N, L) :- fd_domain(L, 1, N).

% Helper predicate that reverses a matrix in left right orientation per row
reverse_matrix([], []).
reverse_matrix([H|T], [RevH|RevT]) :- reverse(H, RevH), reverse_matrix(T, RevT).

% Helper predicate that checks a row is valid given hint count (on relative left side)
valid_row(Iter, [], _, CountTarget) :- Iter = CountTarget.
valid_row(Iter, [H|T], Peak, CountTarget) :-
	H < Peak,
	valid_row(Iter, T, Peak, CountTarget).
valid_row(Iter, [H|T], Peak, CountTarget) :- 
	H > Peak,
	NextIter is Iter + 1,
	valid_row(NextIter, T, H, CountTarget).

% Helper predicate that checks a list of counts (on relative left side) is valid for matrix
valid_counts([], []).
valid_counts([H|T], [HRow|TRows]) :- valid_row(0, HRow, 0, H), valid_counts(T, TRows).


% tower/3 predicate, utilizing GNU Prolog finite domain solver
tower(N, T, C) :- 
	length(T, N),
	rows_length(T, N),
	transpose(T, T_t),
	C = counts(Top, Bot, Left, Right),
	length(Top, N),
	length(Bot, N),
	length(Left, N),
	length(Right, N),
	maplist(list_range(N), T),
	maplist(fd_all_different, T),
	maplist(fd_all_different, T_t),
	maplist(fd_labeling, T),
	valid_counts(Left, T),
	reverse_matrix(T, Trev),
	valid_counts(Right, Trev),
	valid_counts(Top, T_t),
	reverse_matrix(T_t, T_trev),
	valid_counts(Bot, T_trev).


% Additional helper predicates for solving tower puzzle without FD solver

% Helper predicate that creates a list between 1-N, then permutates this list to label a row
label_range(N, L) :- findall(Num, between(1, N, Num), Bag), permutation(L, Bag).

% Helper predicate that checks if a given row is all unique
all_unique([]).
all_unique([H|T]) :- member(H, T), !, fail.
all_unique([H|T]) :- all_unique(T).

% Helper predicate that checks two lists have different values at every index, i.e. unique columns.
unique_columns([], []).
unique_columns([H1|T1], [H2|T2]) :- H1 \= H2, unique_columns(T1, T2).

% Helper predicate that fills the puzzle with valid rows, that fit constraints
% 1. Within range of 1 and N
% 2. Has unique columns compared to previous rows_length
% 3. Satisfies count constraints on left and right 
fill_puzzle(_, [], _, _, _).
fill_puzzle(N, [H|T], [L|LRest], [R|RRest], PrevPerm) :- 
	permutation(H, PrevPerm),
	unique_columns(H, PrevPerm),
	valid_row(0, H, 0, L),
	reverse(H, Rev),
	valid_row(0, Rev, 0, R),
	fill_puzzle(N, T, LRest, RRest, H).
fill_puzzle(_, [], _, _).
fill_puzzle(N, [H|T], [L|LRest], [R|RRest]) :- 
	findall(Num, between(1, N, Num), Bag), 
	permutation(H, Bag),
	valid_row(0, H, 0, L),
	reverse(H, Rev),
	valid_row(0, Rev, 0, R),
	fill_puzzle(N, T, LRest, RRest, H).



% plain_tower/3 predicate, utilizing GNU Prolog finite domain solver
plain_tower(N, T, C) :- 
	length(T, N),
	rows_length(T, N),
	transpose(T, T_t),
	C = counts(Top, Bot, Left, Right),
	length(Top, N),
	length(Bot, N),
	length(Left, N),
	length(Right, N),
	fill_puzzle(N, T, Left, Right),
	maplist(all_unique, T_t),
	valid_counts(Top, T_t),
	reverse_matrix(T_t, T_trev),
	valid_counts(Bot, T_trev).


% Performance comparison predicates and helper predicates

tower_time(Time) :-
	statistics(cpu_time, [_|_]),
	tower(5, T, counts([T1, T2, T3, 3, T5], [B1, B2, 4, B4, 1], [2, L2, L3, L4, 2], [3, 2, R3, 2, R5])),
	statistics(cpu_time, [_|[Time]]).

plain_tower_time(Time) :-
	statistics(cpu_time, [_|_]),
	plain_tower(5, T, counts([T1, T2, T3, 3, T5], [B1, B2, 4, B4, 1], [2, L2, L3, L4, 2], [3, 2, R3, 2, R5])),
	statistics(cpu_time, [_|[Time]]).

speedup(R) :- 
	tower_time(TT),
	plain_tower_time(PTT),
	R is PTT / TT.


% Predicate for finding ambiguous tower games
ambiguous(N, C, T1, T2) :-
	tower(N, T1, C),
	tower(N, T2, C),
	T1 \= T2.