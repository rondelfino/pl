type student_id = int
type grade = int (* must be in 0 to 100 range *)
type final_grade = { id : student_id, grade : grade option }
datatype pass_fail = pass | fail

(* final_grade -> pass_fail*)
(* return pass if the grade field contains SOME i for an i >= 75 else fail *)
fun pass_or_fail {grade, id} =
	case grade of
		 SOME i => if i >= 75 then pass else fail
	   | NONE => fail
		 

(* final_grade -> bool *)
(* return true iff the grade field contains SOME i for an i >= 75 *)
fun has_passed {grade, id} =
	let val final = pass_or_fail {grade = grade, id = id}
	in
		case final of
			 pass => true
    	  |  fail => false
	end


(* (pass_fail * final_grade) list -> int *)
(* indicates how many items in the list were mislabled *)
(* mislabeling means a pair (pass,x) where has_passed x is false or (fail,x) where has_passed x is true.*)
fun number_misgraded (grade_pairs) =
	let fun helper (count, pairs) = 
			 case pairs of
			       [] => count
				 | (pof,{grade,id})::gp => let val check = has_passed {grade=grade,id=id}
									           val test = if pof=pass then true else false
									 in 
										 if test andalso check
										 then helper (count, gp) else helper (1 + count, gp)
									 end
	in
		helper (0, grade_pairs)
	end


datatype 'a tree = leaf 
                 | node of { value : 'a, left : 'a tree, right : 'a tree }
datatype flag = leave_me_alone | prune_me

fun max (a,b) =
	if a > b then a else b

(* 'a tree -> int *)
(* return the height of an 'a tree *)
fun tree_height tree =
	case tree of
		 leaf => 0
	   | node i => let val {value=v,left=l,right=r} = i
					   val height_l = tree_height l
					   val height_r = tree_height r
				   in
					   1 + max (height_l,  height_r)
				   end
						
					    
(* int tree -> int *)
(* sum the values of an int tree *)
fun sum_tree tree =
	case tree of
		 leaf => 0
	   | node i => let val {value=v,left=l,right=r} = i
					   val sum_l = sum_tree l
					   val sum_r = sum_tree r
				   in
						v + sum_l + sum_r
				   end

(* flag tree -> flag tree *)
(* return a flag tree with all its prune_me flags and their descendants removed *)
fun gardener tree =
	case tree of
	     leaf => leaf
		| node i => let val {value=v,left=l,right=r} = i
						val trim_l = gardener l
						val trim_r = gardener r
					in
						if v=prune_me then leaf
									  else node { value=v, left=trim_l, right=trim_r }
					end

exception Empty

(* 'a list -> 'a *)
(* return last element of list, raises Empty if l is nil *)
fun last l =
	case l of
		 [] => raise Empty
	   | e::[] => e
	   | _::l' => last l'


exception Subscript

(* 'a list * int -> 'a list *)
(* returns the first i elements of the list l. It raises Subscript if i < 0 or i > length l. take(l, length l) = l *)
fun take (l, i) =
	if (i < 0) orelse (i > length l) then raise Subscript
	else
	case (l, i) of
		 (_, 0) => []
	   | (first::rest, i) => first :: take (rest, i - 1)


(* 'a list * int -> 'a list *)
(* returns what is left after dropping the first i elements in l. It raises Subscript if i < 0 or i > length l *)
fun drop (l, i) =
	if (i < 0) orelse (i > length l) then raise Subscript
	else
	case (l, i) of
		 (l', 0) => l'
	   | (_::l', i) => drop (l', i - 1) 


(* 'a list list -> 'a list *)
(* returns the list that is the concatination of all the lists in l in order *)
fun concat l =
	case l of
		 [] => []
	   | x::xs => x @ concat xs


(* 'a option * 'a -> 'a '*)
(* returns v if opt is SOME v; otherwise it returns a *)
fun getOpt (opt, a) =
	case opt of
		 SOME v => v
	   | NONE => a


(* 'a option option -> 'a option *)
(* unwrap an option *)
fun join opt =
	case opt of
		 SOME v => v
	   | NONE => NONE



datatype nat = ZERO | SUCC of nat

(* nat -> bool *)
(* return true if natural is positive *)
fun is_positive n =
	case n of
		 ZERO => false
	   | _ => true


exception Negative

(* nat -> nat *)
(* return the predecessor of given nat *)
(* Negative is raised if less than 0 *)
fun pred n =
	case n of
		 ZERO => raise Negative
	   | SUCC n' => n'

(* nat -> int *)
(* convert nat to int *)
fun nat_to_int n =
	case n of
		 ZERO => 0
	   | SUCC n' => 1 + nat_to_int n'


(* int -> nat *)
(* convert int to nat *)
fun int_to_nat i =
	case i of
		0 => ZERO
	  | i' => if i < 0 then raise Negative else SUCC (int_to_nat (i - 1))


(* nat * nat -> nat *)
(* add two naturals *)
fun add (x, y) =
	let fun helper (n, sum) =
			case n of
				 ZERO => sum
			   | SUCC n' => helper (n', SUCC(sum))
	in
		helper (x, y)
	end
		

(* nat * nat -> nat *)
(* subtract y from x *)
fun sub (x, y) =
	let fun helper (n, sum) =
			case n of
				 ZERO => sum
			   | SUCC n' => helper (n', pred sum)
	in
		helper (y, x)
	end


(* nat * nat -> nat *)
(* multiply x and y *)
fun mult (x, y) = 
	let fun helper (n, prod) =
		case (x, n) of
			 (ZERO,_) => ZERO
		   | (_, ZERO) => ZERO
		   | (_, SUCC ZERO) => prod
		   | (_, SUCC n') => helper (n', add (x, prod))
	in
		helper (y, x)
	end


(* nat * nat -> bool *)
(* produce true if x less than y *)
fun less_than (x, y) = 
	(sub (x, y); false)
	handle Negative => true


datatype intSet = 
  Elems of int list (*list of integers, possibly with duplicates to be ignored*)
| Range of { from : int, to : int }  (* integers from one number to another *)
| Union of intSet * intSet (* union of the two sets *)
| Intersection of intSet * intSet (* intersection of the two sets *)


(* intSet -> bool *)
(* return true if set is empty *)
fun isEmpty s =
	case s of
		 Elems [] => true
	   | Elems _ => false
	   | Range { from, to } => from > to
	   | Union (s1, s2) => isEmpty s1 andalso isEmpty s2
	   | Intersection (s1, s2) => isEmpty s1 orelse isEmpty s2 (* does not cover the case where s1 and s2 do not have any similar elements (intersection is empty) *)


(* intSet * int -> bool *)
(* return true if n is contained in s *)
fun contains (s, n) =
	case s of
		 Elems (x::xs) => x=n orelse contains (Elems xs, n)
	   | Elems [] => false
	   | Range { from, to } => n >= from andalso n <= to
	   | Union (s1, s2) => contains (s1, n) orelse contains (s2, n)
	   | Intersection (s1, s2) => contains (s1, n) andalso contains (s2, n)


(* intSet -> int list *)
(* convert s to an int list without duplicates *)
fun toList s =
	let fun get_intersection (l1, l2) =
			case (l1, l2) of
				 ([],_) => []
			   | (_,[]) => []
			   | (x::xs, _::ys) => if contains (Elems l2, x) then x :: get_intersection (xs, ys) else get_intersection (xs, l2)
			
		fun helper (set, reslist) =
			if (isEmpty set) then reslist
			else
			case set of
				 Elems (x::xs) => if contains (Elems reslist, x) then helper (Elems xs, reslist) else helper (Elems xs, reslist@[x])
			   | Range { from, to } => if contains (Elems reslist, from) then helper ((Range { from=from+1, to=to }), reslist)
													    			     else helper ((Range { from=from+1, to=to }), reslist@[from])
			   | Union (s1, s2) => let val list1 = helper (s1, reslist) in helper (s2, list1) end
	 		   | Intersection (s1, s2) => let val list1 = helper (s1, reslist) val list2 = helper (s2, reslist)
										  in
											 get_intersection (list1, list2)
										  end
	in
		helper (s, [])
	end


