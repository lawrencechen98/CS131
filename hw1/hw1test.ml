let my_subset_test0 = subset [] []
let my_subset_test1 = subset [] [1; 1; 1]
let my_subset_test2 = not (subset [1; 2] [])

let my_equal_sets_test0 = equal_sets [] []
let my_equal_sets_test1 = equal_sets [1; 1] [1; 1]
let my_equal_sets_test1 = not (equal_sets [1; 2] [1; 1])

let my_set_union_test0 = equal_sets (set_union [1;2] [1;3]) [1;2;3]
let my_set_union_test1 = equal_sets (set_union [1;2;3] [4;5;6]) [1;2;3;4;5;6]
let my_set_union_test2 = equal_sets (set_union [1] [1]) [1]

let my_set_intersection_test0 =
  equal_sets (set_intersection [1;2] []) []
let my_set_intersection_test1 =
  equal_sets (set_intersection [1;2;3;4;5] [4;5;6;7]) [4;5]
let my_set_intersection_test2 =
  equal_sets (set_intersection [1;2;3;4] [5;6;7]) []

let my_set_diff_test0 = equal_sets (set_diff [] []) []
let my_set_diff_test1 = equal_sets (set_diff [1;3] []) [1;3]
let my_set_diff_test2 = equal_sets (set_diff [1;3] [1;3]) []

let my_computed_fixed_point_test0 =
  computed_fixed_point (=) exp 1.5 = infinity
let my_computed_fixed_point_test1 =
  computed_fixed_point (=) sqrt 1000000000. = 1.

type my_nonterminals =
  | A | B | C | D | E | F

let my_rules =
  [A, [T"a"; N A; N B];
   B, [N D];
   C, [N A; T"c"];
   D, [N E];
   D, [N B];
   E, [N A];
   F, [N C]]

let my_filter_reachable_test0 =
  filter_reachable (A, my_rules) = (A, 
  [A, [T"a"; N A; N B];
   B, [N D];
   D, [N E];
   D, [N B];
   E, [N A]])

let my_filter_reachable_test1 = 
  filter_reachable (C, my_rules) = (C,
  [A, [T"a"; N A; N B];
   B, [N D];
   C, [N A; T"c"];
   D, [N E];
   D, [N B];
   E, [N A]])

let my_filter_reachable_test2 =
  filter_reachable (F, my_rules) = (F, my_rules)