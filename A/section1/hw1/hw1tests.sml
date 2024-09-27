(* Homework1 Tests *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

use "hw1.sml";

(* is_older (d1: int*int*int, d2: int*int*int) *)
val io1 = is_older ((1,2,3),(1,2,3)) = false
val io2 = is_older ((2012,2,28),(2013,12,1)) = true
val io3 = is_older ((2012,2,28),(2012,2,28)) = false
val io4 = is_older ((2011,3,28),(2011,2,28)) = false
val io5 = is_older ((2011,2,27),(2011,2,28)) = true
val io6 = is_older ((2011,3,31),(2011,4,28)) = true
val io7 = is_older ((2023,9,5),(2023,8,15)) = false

(* number_in_month (lod: (int*int*int) list, month: int) *)
val nim1 = number_in_month ([],1) = 0
val nim2 = number_in_month ([(2012,2,28),(2013,12,1)],2) = 1

(* number_in_months (lod: (int*int*int) list, lom: int list) *)
(*        lom     []     int :: lom *)
(* lod                              *)
(* []             0           0     *)
(* Date :: lod    0    number_in_month (lod, hd lom) + 
                       natural recursion *)
val nims1 = number_in_months ([],[2,3,4]) = 0
val nims2 = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[]) = 0
val nims3 = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = 3

(* dates_in_month (lod: (int*int*int) list, month: int) *)
val dim1 = dates_in_month ([],2) = []
val dim2 = dates_in_month ([(2012,2,28),(2013,12,1)],2) = [(2012,2,28)]

(* dates_in_months (lod: (int*int*int) list, lom: int list) *)
(*        lom     []     int :: lom *)
(* lod                              *)
(* []             []         []     *)
(* Date :: lod    []   dates_in_month (lod, hd lom) @
                       natural recursion *)
val dims1 = dates_in_months ([],[2,3,4]) = []
val dims2 = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[]) = []
val dims3 = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)]

(* get_nth (los: string list, n: int) *)
val n1 = get_nth (["hi", "there", "how", "are", "you"], 1) = "hi"
val n2 = get_nth (["hi", "there", "how", "are", "you"], 2) = "there"

(* date_to_string (date: int*int*int) *)
val dts1 = date_to_string (2013, 6, 1) = "June 1, 2013"

(* number_before_reaching_sum (sum: int, xs: int list) *)
val nbrs1 = number_before_reaching_sum (10, []) = 0 
val nbrs2 = number_before_reaching_sum (10, [1,2,3,4,5]) = 3
val nbrs3 = number_before_reaching_sum (50, [1,2,3,4,5]) = 5

(* what_month (doy: int) *)
val wm1 = what_month 70 = 3

(* month_range (day1: int, day2: int) *)
val mr1 = month_range (34, 31) = []
val mr2 = month_range (31, 34) = [1,2,2,2]

(* oldest (lod: (int*int*int) list) *)
val oldest1 = oldest([(2012,2,28),(2011,3,31),(2011,4,28)]) = SOME (2011,3,31)
val oldest2 = oldest([]) = NONE

(* number_in_months_challenge (lod: (int*int*int) list, lom: int list) *)
val nimsc1 = number_in_months_challenge ([],[2,3,4]) = 0
val nimsc2 = number_in_months_challenge ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[]) = 0
val nimsc3 = number_in_months_challenge ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = 3
val nimsc4 = number_in_months_challenge ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4,4,5]) = 3

(* dates_in_months_challenge (lod: (int*int*int) list, lom: int list) *)
val dimsc1 = dates_in_months_challenge ([],[2,3,4]) = []
val dimsc2 = dates_in_months_challenge ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[]) = []
val dimsc3 = dates_in_months_challenge ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)]
val dimsc4 = dates_in_months_challenge ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4,4,5]) = [(2012,2,28),(2011,3,31),(2011,4,28)]

(* fun reasonable_date (date: int*int*int) *)
val rd1 = reasonable_date ((1,2,3)) = true
val rd2 = reasonable_date ((2025,2,29)) = false
val rd3 = reasonable_date ((2024,2,29)) = true
val rd4 = reasonable_date ((1523,13,19)) = false
val rd5 = reasonable_date ((1523,12,19)) = true
val rd6 = reasonable_date ((1523,9,31)) = false
val rd7 = reasonable_date ((0,2,3)) = false
