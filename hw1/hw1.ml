type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

(* import module List *)
open List

let rec subset a b = 
	match a with
	| [] -> true (* empty set is a subset of any set *)
	| first :: rest -> 
		if (mem first b) then (subset rest b) else false

let equal_sets a b = 
	subset a b && subset b a (* sets are equal iff they are subsets of each other *)

let rec set_union a b = 
	match a with
	| [] -> b (* union of a set with an empty set is the set itself *)
	| first :: rest -> if (mem first b) then (set_union rest b) else (set_union rest (first::b))

let rec set_intersection a b = 
	match a with 
	| [] -> [] (* intersection of a set with an empty set is the empty set *)
	| first :: rest -> if (mem first b) then (first :: (set_intersection rest b)) else (set_intersection rest b)

let rec set_diff a b = 
	match a with
	| [] -> [] (* set difference of an empty set with any set is an empty set *)
	| first :: rest -> if (mem first b) then (set_diff rest b) else (first :: (set_diff rest b))

let rec computed_fixed_point eq f x =
	if (eq (f x) x) then x else (computed_fixed_point eq f (f x))

(* get list of all symbols on the right hand side of rules that can be reached with non-terminal symbols in visited*)
let rec get_reachable_rhs rules visited = 
	match rules with 
	| [] -> []
	| first :: rest -> if (mem (fst first) visited) then (set_union (snd first) (get_reachable_rhs rest visited)) else (get_reachable_rhs rest visited)

(* remove all terminal symbols in symbols, return only nonterminal symbols *)
let rec filter_nonterminal symbols = 
	match symbols with
	| [] -> []
	| first :: rest -> 
		match first with
		| T t -> (filter_nonterminal rest)
		| N n -> n :: (filter_nonterminal rest)

(* evaluates rules using the symbols that are in visited, returning all non terminal symbols that can be reached *)
let evaluate_rules rules visited = 
	(set_union visited (filter_nonterminal (get_reachable_rhs rules visited)))

(* get all rules that can be reached with symbols of visited, i.e. rules with left hand side symbol in visited *)
let rec get_reachable_rules rules visited = 
	match rules with 
	| [] -> []
	| first :: rest -> if (mem (fst first) visited) then first :: (get_reachable_rules rest visited) else (get_reachable_rules rest visited)

(* filter grammar g and returns copy with only reachable rules *)
let rec filter_reachable g = 
	match g with 
	| (start, rules) -> (start, (get_reachable_rules rules (computed_fixed_point equal_sets (evaluate_rules rules) (start::[]))))







