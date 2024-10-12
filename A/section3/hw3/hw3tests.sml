(* Homework3 Simple Test*)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

use "hw3.sml";

val oc1 = only_capitals ["A","B","C"] = ["A","B","C"]
val oc2 = only_capitals ["A","a","C"] = ["A","C"]
val oc3 = only_capitals [] = []

val ls1 = longest_string1 ["A","bc","C"] = "bc"
val ls2 = longest_string1 ["A","bc","cd"] = "bc"

val ls3 = longest_string2 ["A","bc","cd"] = "cd"

val ls4a = longest_string3 ["A","bc","C"] = "bc"
val ls4b = longest_string3 ["A","bc","cd"] = "bc"
val ls4c = longest_string4 ["A","B","C"] = "C"

val lsh1 = longest_string_helper op> ["this","list","has","no","capital","letters"] = "capital"

val lc1 = longest_capitalized ["A","bc","C"] = "A"
val lc2 = longest_capitalized ["a","bc","c"] = ""

val rs1 = rev_string "abc" = "cba"

val fa1 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4

val aa1 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE
val aa2 = all_answers (fn x => if x < 8 then SOME [x] else NONE) [2,3,4,5,6,7] = SOME [2,3,4,5,6,7]
val aa3 = all_answers (fn x => if x < 8 then SOME [x] else NONE) [] = SOME []

val cw1 = count_wildcards Wildcard = 1
val cw2 = count_wildcards (TupleP [Wildcard,Wildcard]) = 2
val cw3 = count_wildcards (Variable "hi") = 0

val cwavl1 = count_wild_and_variable_lengths (Variable("a")) = 1
val cwavl2 = count_wild_and_variable_lengths (TupleP [Wildcard,Wildcard,(Variable("hi"))]) = 4

val csv1 = count_some_var ("x", Variable("x")) = 1
val csv2 = count_some_var ("d", Variable("x")) = 0

val cp1 = check_pat (Variable("x")) = true
val cp2 = check_pat (TupleP [Wildcard,(Variable("hi")),(Variable("hello")),(ConstructorP("farts",(Variable("i am stupid"))))]) = true
val cp3 = check_pat (TupleP [Wildcard,(Variable("hi")),(Variable("hi")),(ConstructorP("farts",(Variable("i am stupid"))))]) = false
val cp4 = check_pat Wildcard = true
val cp5 = check_pat (TupleP[TupleP[Variable "x",ConstructorP ("wild",Wildcard)],Variable "x"]) = false
val cp6 = check_pat (TupleP[TupleP[TupleP[Variable "x",ConstructorP ("wild",Wildcard)],Wildcard],Variable "x"]) = false
val cp7 = check_pat (ConstructorP ("hi",TupleP[Variable "x",Variable "x"])) = false
val cp8 = check_pat (ConstructorP ("hi",TupleP[Variable "x",ConstructorP ("yo",TupleP[Variable "x",UnitP])])) = false

val m1 = match (Const(1), UnitP) = NONE
val m2 = match (Const(1), ConstP(1)) = SOME []
val m3 = match (Const(1), Variable("hi")) = SOME [("hi", Const(1))]
val m4 = match ((Tuple [Const(1), Const(2)]), (TupleP [Variable("a"), Variable("b")])) = SOME [("a", Const(1)), ("b", Const(2))]
val m5 = match ((Tuple [Const(1), Constructor("b", Const(3))]), (TupleP [Variable("a"), ConstructorP("b", Variable("c"))])) = SOME [("a", Const(1)), ("c", Const(3))]
val m6 = match (Tuple[Const 17,Unit,Const 4,Constructor ("egg",Const 4),Constructor ("egg",Constructor ("egg",Const 4))],TupleP[Wildcard,Wildcard]) = NONE

val fm1 = first_match Unit [UnitP] = SOME []
val fm2 = first_match (Const(1)) ([Variable("a")]) = SOME [("a", Const(1))] 
(* val fm1 = first_match (Const 5) [Wildcard, Variable "x", ConstP 5]           (* Expected: SOME [("x", Const 5)] *) *)
(* val fm2 = first_match (Tuple [Const 1, Const 2]) [Wildcard, TupleP [ConstP 1, Wildcard], ConstP 5]  (* Expected: SOME [] *) *)
(* val fm3 = first_match (Constructor ("Some", Const 5)) [ConstructorP ("Some", ConstP 5), TupleP [Wildcard], Wildcard] (* Expected: SOME [] *) *)
(* val fm4 = first_match Unit [UnitP, ConstP 5, Variable "x"]                   (* Expected: SOME [] *) *)
(* val fm5 = first_match (Const 5) [ConstP 5, TupleP [Wildcard]]                  (* Expected: SOME [] *) *)
(* val fm6 = first_match (Tuple [Const 1, Const 2]) []                             (* Expected: NONE *) *)
(* val fm7 = first_match (Constructor ("Some", Const 5)) [Variable "y"]           (* Expected: NONE *) *)

val tcp1 = typecheck_patterns ([], [TupleP[Variable("x"),Variable("y")], TupleP[Wildcard,Wildcard]]) = SOME (TupleT [Anything, Anything])
val tcp2 = typecheck_patterns ([], [TupleP[Wildcard,Wildcard], TupleP[Wildcard,TupleP[Wildcard,Wildcard]]]) = SOME (TupleT[Anything,TupleT[Anything,Anything]])
val tcp3 = typecheck_patterns ([], [ConstP 10, Variable "a"]) = SOME IntT 
val tcp4 = typecheck_patterns ([], [ConstP 10, Variable "a", ConstructorP("SOME", Variable "x")]) = NONE
val tcp5 = typecheck_patterns ([], [(TupleP[Variable "a", ConstP 10, Wildcard]), (TupleP[Variable "b", Wildcard, ConstP 11]), Wildcard]) = SOME (TupleT[Anything,IntT,IntT])
val tcp6 = typecheck_patterns ([("Red","color",UnitT),("Green","color",UnitT),("Blue","color",UnitT)], [ConstructorP("Red", UnitP), Wildcard]) = SOME (Datatype "color")
val tcp7 = typecheck_patterns ([("Sedan","auto", Datatype "color"),("Truck","auto",TupleT[IntT, Datatype "color"]),("SUV","auto",UnitT)], [ConstructorP("Sedan", Variable "a"), ConstructorP("Truck", TupleP[Variable "b", Wildcard]), Wildcard]) = 
SOME (Datatype "auto")
val tcp8 = typecheck_patterns ([("Empty","list",UnitT),("List","list",TupleT[Anything, Datatype "list"])], [ConstructorP("Empty",UnitP), ConstructorP("List",TupleP[ConstP 10, ConstructorP("Empty",UnitP)]), Wildcard]) = SOME (Datatype "list")
val tcp9 = typecheck_patterns ([("Empty","list",UnitT),("List","list",TupleT[Anything, Datatype "list"])], [ConstructorP("Empty",UnitP),ConstructorP("List",TupleP[Variable "k", Wildcard])]) = SOME (Datatype "list")
val tcp10 = typecheck_patterns ([("Empty","list",UnitT),("List","list",TupleT[Anything, Datatype "list"]), ("Sedan","auto", Datatype "color")], [ConstructorP("Empty",UnitP),ConstructorP("List",TupleP[ConstructorP("Sedan", Variable "c"), Wildcard])]) = SOME (Datatype "list")

