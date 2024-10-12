
use "ep3.sml";

val du1 = do_until (fn x => x div 2) (fn x => x mod 2 <> 1) 64 = 1 
val du2 = do_until (fn x => x div 2) (fn x => x mod 2 <> 1) 50 = 25 

val fact1 = factorial 1 = 1
val fact2 = factorial 5 = 120

val fp1 = fixed_point (fn x => x div 2) 100 = 0

val mp1 = map2 (fn x => x + 2) (45, 30) = (47, 32)

val f = (fn n => [n, n * 2, n * 3]) 
val aa1 = app_all f f 1 = [1, 2, 3, 2, 4, 6, 3, 6, 9]

val foldr1 = foldr op+ 0 [1,2,3] = 6
val foldr2 = foldr (fn (x, acc) => x - acc) 0 [1, 2, 3] = 2

val p1 = partition (fn x => x mod 2 <> 1) [1,2,3,4,5,6,7,8,9,10] = ([2,4,6,8,10], [1,3,5,7,9])

val uf1 = unfold (fn n => if n = 0 then NONE else SOME(n, n-1)) 5 = [5, 4, 3, 2, 1]

val fact3 = fact 1 = 1
val fact4 = fact 5 = 120

val tree = Node (1, (Node (2, Leaf, Leaf)), (Node (3, (Node (4, Leaf, Leaf)), Leaf)))
val mpt1 = map_tree (fn x => x * 2) tree
