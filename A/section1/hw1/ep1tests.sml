
(* Extra Problems Tests *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

use "ep1.sml";

(* alternate (xs: int list) *)
val alt1 = alternate ([]) = 0
val alt2 = alternate ([1]) = 1
val alt3 = alternate ([1,2]) = 1 - 2
val alt4 = alternate ([1,2,3,10]) = 1 - 2 + 3 - 10
val alt5 = alternate ([10,2,3,5]) = 10 - 2 + 3 - 5


(* min_max (xs: int list) *)
val mm1 = min_max ([1,2]) = (1,2)
val mm2 = min_max ([1,3,4,1,2,203,42,~5]) = (~5,203)

(* cumsum (xs: int list) *)
val cs1 = cumsum ([]) = []
val cs2 = cumsum ([1]) = [1]
val cs3 = cumsum ([1,24]) = [1,25]
val cs4 = cumsum ([1,4,20]) = [1,5,25]

(* greeting (s: string option) *)
val g1 = greeting (NONE) = "Hello there you"
val g2 = greeting (SOME "billy") = "Hello there billy"

(* repeat (xs: int list, ys: int list) *)
val r1 = repeat ([],[]) = []
val r2 = repeat ([1],[2]) = [1,1]
val r3 = repeat ([1,2,3],[4,0,3]) = [1,1,1,1,3,3,3]

(* addOpt (n1: int option, n2: int option) *)
val ao1 = addOpt (NONE,NONE) = NONE
val ao2 = addOpt (SOME 5,NONE) = NONE
val ao3 = addOpt (NONE,SOME 5) = NONE
val ao4 = addOpt (SOME 5,SOME 5) = SOME 10

(* addAllOpt (xs: int option list) *)
val aao1 = addAllOpt ([]) = NONE
val aao2 = addAllOpt ([SOME 1,NONE]) = SOME 1
val aao3 = addAllOpt ([SOME 1,NONE,SOME 2,SOME 3]) = SOME 6

(* any (xs: bool list) *)
val any1 = any ([]) = false
val any2 = any ([false]) = false
val any3 = any ([false,false,true]) = true

(* all (xs: bool list) *)
val all1 = all ([]) = true
val all2 = all ([false]) = false
val all3 = all ([false,false,true]) = false
val all4 = all ([true,true,true]) = true

(* zip (xs: int list, ys: int list) *)
val zip1 = zip ([],[]) = []
val zip2 = zip ([],[1,2]) = []
val zip3 = zip ([1,2],[3,4]) = [(1,3),(2,4)]
val zip4 = zip ([1,2,3],[4,6]) = [(1,4),(2,6)]

(* zipRecycle (xs: int list, ys: int list) *)
val zipr1 = zipRecycle ([],[]) = []
val zipr2 = zipRecycle ([],[1,2]) = []
val zipr3 = zipRecycle ([1,2],[3,4]) = [(1,3),(2,4)]
val zipr4 = zipRecycle ([1,2,3],[4,6]) = [(1,4),(2,6),(3,4)]
val zipr5 = zipRecycle ([1,2,3],[1,2,3,4,5,6,7]) = [(1,1),(2,2),(3,3),(1,4),(2,5),(3,6),(1,7)]

(* zipOpt (xs: int list, ys: int list) *)
val zipo1 = zipOpt ([],[]) = SOME []
val zipo2 = zipOpt ([],[1,2]) = NONE
val zipo3 = zipOpt ([1,2],[3,4]) = SOME ([(1,3),(2,4)])
val zipo4 = zipOpt ([1,2,3],[4,6]) = NONE

(* lookup (pairs: (string * int) list, key: string) *)
val lookup1 = lookup ([],"hi") = NONE
val lookup2 = lookup ([("hi",2)],"hi") = SOME 2
val lookup3 = lookup ([("hi",2),("how",3),("are",4),("you",5)],"you") = SOME 5
val lookup4 = lookup ([("hi",2),("how",3),("are",4),("you",5)],"john") = NONE

(* splitup (xs: int list) *)
val split1 = splitup ([]) = ([],[])
val split2 = splitup ([1,~1]) = ([1],[~1])
val split3 = splitup ([1,~1,2,3,~3,~4,5,6,~1]) = ([1,2,3,5,6],[~1,~3,~4,~1])

(* splitAt (xs: int list, index: int) *)
val splita1 = splitAt ([],0) = ([],[])
val splita2 = splitAt ([1,~1],0) = ([1],[~1])
val splita3 = splitAt ([1,~1,2,3,~3,~4,5,6,~1],4) = ([5,6],[1,~1,2,3,~3,~4,~1])
val splita4 = splitAt ([1,~1,2,3,~3,~4,5,6,~1],~1) = ([1,2,3,5,6],[~1,~3,~4,~1])

(* isSorted (xs: int list) *)
val sorted1 = isSorted ([]) = true
val sorted2 = isSorted ([1,2,3]) = true
val sorted3 = isSorted ([1,2,3,4,1]) = false

(* isAnySorted (xs: int list) *)
val sorteda1 = isAnySorted ([]) = true
val sorteda2 = isAnySorted ([1,2,3]) = true
val sorteda3 = isAnySorted ([1,2,3,4,1]) = false
val sorteda4 = isAnySorted ([4,3,2,1]) = true
val sorteda5 = isAnySorted ([4,3,2,1,1]) = true

(* sortedMerge (xs: int list, ys: int list) *)
val sm1 = sortedMerge ([],[]) = []
val sm2 = sortedMerge ([1],[]) = [1]
val sm3 = sortedMerge ([1,2,3,4],[]) = [1,2,3,4]
val sm4 = sortedMerge ([1,2,3,4],[1,2,5,6,7]) = [1,1,2,2,3,4,5,6,7]

(* qsort (xs: int list) *)
val qsort1 = qsort ([]) = []
val qsort2 = qsort ([1]) = [1]
val qsort3 = qsort ([1,4,5,~3,2,45,~34]) = [~34,~3,1,2,4,5,45]

(* divide (xs: int list) *)
val div1 = divide ([]) = ([],[])
val div2 = divide ([1]) = ([1],[])
val div3 = divide ([1,2,3,4,5,6,7]) = ([1,3,5,7],[2,4,6])

(* not_so_quick_sort (xs: int list) *)
val nsqs1 = not_so_quick_sort ([]) = []
val nsqs2 = not_so_quick_sort ([1]) = [1]
val nsqs3  = not_so_quick_sort ([1,4,5,~3,2,45,~34]) = [~34,~3,1,2,4,5,45]

(* fullDivide (k: int, n: int) *)
val fdiv1 = fullDivide (0,0) = (0,0)
val fdiv2 = fullDivide (3,10) = (0,10)
val fdiv3 = fullDivide (2,40) = (3,5)

(* factorize (n: int) *)
val factorize1 = factorize 1 = []
val factorize2 = factorize 20 = [(2,2),(5,1)]
val factorize3 = factorize 36 = [(2,2),(3,2)]
val factorize4 = factorize 49 = [(7,2)]

(* multiply (n: (int * int) list) *)
val mult1 = multiply [] = 1
val mult2 = multiply [(2,2),(5,1)] = 20
val mult3 = multiply [(2,2),(3,2)] = 36
val mult4 = multiply [(7,2)] = 49

(* all_products (factors: (int * int) list) *)
val ap1 = all_products [] = [1]
val ap2 = all_products [(2,3)] = [1,2,4,8]
val ap3 = all_products [(2,2),(5,1)] = [1,2,4,5,10,20]
val ap4 = all_products [(2,2),(3,1),(5,1)] = [1,2,3,4,5,6,10,12,15,20,30,60]
