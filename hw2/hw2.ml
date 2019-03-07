(* import module List *)
open List

(* define symbol type *)
type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

(* define parse_tree type *)
type ('nonterminal, 'terminal) parse_tree =
  | Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list
  | Leaf of 'terminal

(* takes a nonterminal symbol, and returns list of rules associated to said symbol *)
let rec evaluate_nonterminal nonterminal rules =
	match rules with
	| [] -> []
	| (lhs,rhs)::tail -> 
		if nonterminal==lhs then rhs::(evaluate_nonterminal nonterminal tail) else (evaluate_nonterminal nonterminal tail)

(* calls evaluate_nonterminal helper function and returns the production function as tuple with first symbol *)
let convert_grammar gram1 = 
	(fst gram1, (fun nonterminal -> evaluate_nonterminal nonterminal (snd gram1)))

(* takes a list of parse_tree trees and recusrively interates through leaf values from left to right *)
let rec parse_trees trees =
	match trees with
	| [] -> []
	| head::tail -> 
		match head with 
		| Leaf leaf -> leaf::(parse_trees tail)
		| Node (_, subtree) -> (parse_trees subtree)@(parse_trees tail)

(* calls parse_trees helper function with tree inside a list *)
let parse_tree_leaves tree = 
	parse_trees [tree]

(* generic matcher, takes rule production function, rules, acceptor and fragment, 
	returns tuple (derivation, suffix) from acceptor function *)
let rec matcher rule_func rules = 
	fun acc frag -> 
	(* if more than one rule option, at least one rule of rhs needs to match *)
	if (length rules) > 1 then match_per_rhs rule_func rules acc frag
	(* if more than one term inside rule, all symbols/terms needs to match *)
	else if (length (hd rules)) > 1 then match_per_symbol rule_func (hd rules) acc frag
	(* otherwise there is only one remaining symbol *)
	else match (hd (hd rules)) with
	(* if symbol is nonterminal, use production function to get new set of rules,
		and remember that derivation now is a subtree of a new Node of symbol *)
	| N n -> let new_acc = fun derivation suffix -> (acc [Node(n, derivation)] suffix) 
		in matcher rule_func (rule_func n) new_acc frag 
	(* if symbol is terminal, then call acceptor function with derivation containing current symbol/leaf
		and the suffix, i.e. remaining fragment *)
	| T t -> (match frag with
		| [] -> ([Leaf t], None)
		| frag_head::frag_tail -> if frag_head = t then (acc [Leaf t] frag_tail) else ([Leaf t], None))

(* essentially a make_or_matcher shown in class, which needs to match one of the list of rules given *)
and match_per_rhs rule_func rules =
	match rules with
	| [] -> (fun acc frag -> ([], None))
	| first_rule::rest_rules -> 
		let first_matcher = matcher rule_func [first_rule] 
		and rest_matcher = matcher rule_func rest_rules
		in fun acc frag ->
			let first_match = first_matcher acc frag
			in match (snd first_match) with
			| None -> rest_matcher acc frag
			| x -> first_match

(* essentially a make_and_matcher shown in class, which needs to match all symbols in the single rule given *)
and match_per_symbol rule_func rule acc =
	match rule with
	| [] -> (fun frag -> ([], None))
	| first_symbol::rest_symbols ->
		let first_matcher = matcher rule_func [[first_symbol]] 
		and rest_matcher = matcher rule_func [rest_symbols] 
		(* need to concatenate the derivation of remaining symbols with the current derivation, 
			so wrap acceptor in a new acceptor function that appends the parse_trees together *)
		in let new_acc derivation suffix = (rest_matcher (fun d s -> (acc (derivation@d) s)) suffix)
		in fun frag -> first_matcher new_acc frag

(* make_matcher calls helper function matcher to return accepted suffix *)
let make_matcher gram acc frag =
	let rule_func = snd gram
	and start = fst gram 
	(* write a new acceptor to wrap around given acceptor, so our acceptor accepts
		derivation parse trees too, makes it easier to use for make_parser, but is ignored here *)
	and derivation_acceptor derivation input = (derivation, (acc input))
	(* take only snd item of tuple, ignoring the parse trees *)
	in snd (matcher rule_func (rule_func start) derivation_acceptor frag)

(* make_parser uses same helper function matcher but returns the first item in the tuple,
	i.e. the parse_tree of the derivation *)
let make_parser gram frag = 
	let rule_func = snd gram
	and start = fst gram 
	and acc = (function | _::_ -> None | x -> Some x)
	(* acceptor function wrapper like before *)
	in let derivation_acceptor derivation input = (derivation, (acc input))
	in let result = (matcher rule_func (rule_func start) derivation_acceptor frag) 
	in match result with 
	(* match the derivation parse_tree, should be a list of leaves, so make a new parse tree
		with top node as the start symbol, and the leaves as the returned derivation *)
	(* only returns the tree if the suffix accepted by the acceptor is empty, otherwise
		there was no match, return None *)
	| (derivation, Some x) -> if x = [] then Some (Node (start, derivation)) else None
	| (_, None) -> None