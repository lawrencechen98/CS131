let accept_empty_suffix = function
   | _::_ -> None
   | x -> Some x

type my_nonterminals =
  | Expr | Term | Power | Op | Num

let my_grammar =
  (Expr,
   function
     | Expr ->
         [[N Term; N Op; N Expr];
          [N Term]]
     | Term ->
     [[N Num];
      [N Num; N Power];
      [T"("; N Expr; T")"];
      [T"["; N Expr; T"]"]]
     | Power ->
     [[T"^"; N Expr]]
     | Op ->
     [[T"+"];
      [T"-"];
      [T"*"];
      [T"/"]]
     | Num ->
     [[T"0"]; [T"1"]; [T"2"]; [T"3"]; [T"4"];
      [T"5"]; [T"6"]; [T"7"]; [T"8"]; [T"9"]])

let my_frag = ["("; "8"; "^"; "9"; ")"; "-"; "["; "("; "9"; "+"; "9"; ")";
      "*"; "2"; "]"; "*"; "("; "3"; "+"; "("; "7"; ")"; "-"; "9"; ")";
      "-"; "("; "("; "4"; ")"; ")"; "/"; "("; "1"; ")"]

let make_matcher_test =
 ((make_matcher my_grammar accept_empty_suffix) my_frag = Some [])


let make_parser_test =
  match make_parser my_grammar my_frag with
    | Some tree -> parse_tree_leaves tree = my_frag
    | _ -> false
