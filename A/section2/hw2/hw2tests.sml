(* Homework2 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

use "hw2.sml";

val aeo1 = all_except_option ("a", []) = NONE
val aeo2 = all_except_option ("string", ["string"]) = SOME []
val aeo3 = all_except_option ("a", ["a","b","c"]) = SOME ["b","c"]
val aeo4 = all_except_option ("b", ["a","b","c"]) = SOME ["a","c"]

val gs1 = get_substitutions1 ([["foo"],["there"]], "foo") = []
val gs2 = get_substitutions1([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "Fred") = 
["Fredrick","Freddie","F"]
val gs3 = get_substitutions1([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]], "Jeff") = 
["Jeffrey","Geoff","Jeffrey"]


val gs4 = get_substitutions2 ([["foo"],["there"]], "foo") = []
val gs5 = get_substitutions2([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "Fred") = 
["Fredrick","Freddie","F"]
val gs6 = get_substitutions2([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]], "Jeff") = 
["Jeffrey","Geoff","Jeffrey"]


val sn1 = similar_names ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"}) =
	    [{first="Fred", last="Smith", middle="W"}, {first="Fredrick", last="Smith", middle="W"},
	     {first="Freddie", last="Smith", middle="W"}, {first="F", last="Smith", middle="W"}]
val sn2 = similar_names ([], {first="Fred", middle="W", last="Smith"}) =
	    [{first="Fred", last="Smith", middle="W"}]
	     

val cc1 = card_color (Clubs, Num 2) = Black
val cc2 = card_color (Diamonds, Num 2) = Red

val cv1 = card_value (Clubs, Num 2) = 2
val cv2 = card_value (Clubs, Ace) = 11
val cv3 = card_value (Clubs, Jack) = 10

val rc1 = remove_card ([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = []
val rc2 = remove_card ([(Hearts, Ace), (Hearts, Ace)], (Hearts, Ace), IllegalMove) = [(Hearts, Ace)]
val rc3 = remove_card ([(Hearts, Ace), (Hearts, Ace), (Spades, King), (Diamonds, Num 5)], (Hearts, Ace), IllegalMove) = 
[(Hearts, Ace), (Spades, King), (Diamonds, Num 5)]

val asc1 = all_same_color [] = true
val asc2 = all_same_color [(Hearts, Ace), (Hearts, Ace)] = true
val asc3 = all_same_color [(Hearts, Ace), (Hearts, Ace), (Clubs, Num 2)] = false
val asc4 = all_same_color [(Hearts, Ace), (Clubs, Num 2), (Hearts, Ace), (Diamonds, Num 2), (Clubs, Num 2)] = false

val sc1 = sum_cards [] = 0
val sc2 = sum_cards [(Clubs, Num 2),(Clubs, Num 2)] = 4

val s1 = score ([(Hearts, Num 2),(Clubs, Num 4)],10) = 4
val s2 = score ([],10) = 5

val o1 = officiate ([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6

val o2 = officiate ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                        [Draw,Draw,Draw,Draw,Draw],
                        42)
             = 3

val o3 = ((officiate([(Clubs,Jack),(Spades,Num(8))],
                         [Draw,Discard(Hearts,Jack)],
                         42);
               false) 
              handle IllegalMove => true)
val o4 = officiate ([(Hearts, Ace)], [Draw], 10) = 1  (* 3 * (11 - 10) = 3 *)
val o5 = officiate ([], [Draw], 10) = 5 
val o6 = officiate ([(Hearts, Num 3), (Spades, Num 2)], [Draw, Draw, Discard(Hearts, Num 3)], 15) = 6
val o7 = officiate ([(Clubs, Num 3), (Clubs, Num 4)], [Draw, Draw], 15) = 4  (* (15 - 7) div 2 = 4 *)
val o8 = officiate ([(Clubs, Num 3), (Clubs, Num 4), (Clubs, Ace), (Hearts, Ace)], [Draw, Draw, Draw, Draw], 10) = 12

val schallenge1 = score_challenge ([(Hearts, Num 2),(Clubs, Num 4)],10) = 4
val schallenge2 = score_challenge ([(Hearts, Ace), (Clubs, Num 3)], 15) = 1
(* sum = 11 + 3 = 14, goal = 15, score = 15 - 14 = 1 *)
val schallenge3 = score_challenge ([(Hearts, Ace), (Clubs, Num 9)], 10) = 0
(* sum = 1 + 9 = 10, goal = 10, score = 10 - 10 = 0 *)
val schallenge4 = score_challenge ([(Hearts, Ace), (Clubs, Ace), (Spades, Num 4)], 17) = 1
(* sum = 11 + 1 + 4 = 16, goal = 17, score = 17 - 16 = 1 *)
val schallenge5 = score_challenge ([(Hearts, Ace), (Clubs, Ace), (Diamonds, Ace)], 5) = 2
(* sum = 1 + 1 + 1 = 3, goal = 5, score = 5 - 3 = 2 *)
val schallenge6 = score_challenge ([(Hearts, Ace), (Clubs, Ace), (Spades, King)], 20) = 6
(* sum = 1 + 1 + 10 = 12, goal = 20, score = 3 * (22-20) = 6 *)
val schallenge7 = score_challenge ([(Clubs, Ace), (Clubs, Ace), (Spades, King)], 21) = 1
(* sum = 11 + 1 + 10 = 12, goal = 21, score = 3 * (22-21) = 3, divided by 2 = 1 *)
val schallenge8 = score_challenge ([], 21) = 10

val ochallenge1 = officiate_challenge ([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6
val ochallenge2 = officiate_challenge ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                        [Draw,Draw,Draw,Draw,Draw],
                        42)
             = 3
val ochallenge3 = officiate_challenge ([(Clubs, Ace), (Hearts, Ace)], [Draw, Draw], 20) = 6
val ochallenge4 = officiate_challenge ([(Clubs, Ace), (Clubs, Ace), (Spades, King)], [Draw, Draw, Draw], 20) = 3
(* sum = 1 + 11 + 10 = 22; goal = 20; score = 3 * (22 - 20) = 6, divided by 2 = 3 *)
(* val ochallenge5 = officiate_challenge ([(Hearts, Ace), (Clubs, Num 3), (Diamonds, Num 5)], [Draw, Discard(Clubs, Num 3), Draw], 15) = 1 *)
(* sum = 11 (Ace) + 5 = 16; goal = 15; score = 3 * (16 - 15) = 3, divided by 2 = 1 *)
val ochallenge5 = officiate_challenge ([], [Draw], 10) = 5 


val cp1 = careful_player ([], 11) = []
val cp2 = careful_player ([(Clubs, Ace)], 20) = [Draw]
val cp3 = careful_player ([(Clubs, Ace), (Diamonds, Ace), (Spades, King)], 20) = [Draw]
val cp5 = careful_player ([(Clubs, Ace), (Hearts, Ace)], 11) = [Draw] 
val cp6 = careful_player ([(Hearts, Ace), (Diamonds, Ace)], 22) = [Draw, Draw]
val cp7 = careful_player ([(Spades, King)], 10) = []
val cp8 = careful_player ([(Hearts, Num 8), (Clubs, Num 9)], 18) = [Draw, Discard (Hearts, Num 8), Draw]
