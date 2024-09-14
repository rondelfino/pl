(* int list -> int*)
(* adds the elements in the given list of numbers with an alternating sign *)
fun alternate (xs: int list) =
	if null xs
	then 0
	else hd xs - alternate (tl xs)


(* int list -> int*int *)
(* produce a pair (min,max) of the minimum and maximum numbers in the given non-empty list *)
fun min_max (xs: int list) =
	let fun helper (current_min: int, current_max: int, xs: int list) =
			if null xs
			then (current_min,current_max)
			else helper (if hd xs < current_min then hd xs else current_min, 
						 if hd xs > current_max then hd xs else current_max, 
						 tl xs)
	in
		helper (hd xs, hd xs, tl xs)
	end


(* int list -> int *)
(* return a list of the partial sums of a given list of numbers *)
fun cumsum (xs: int list) = 
	let fun helper (prev: int, xs: int list) =
			if null xs
			then []
			else let val current = prev + hd xs 
				 in
					 current :: helper (current, tl xs)
				 end
	in
		helper (0,xs)
	end


(* string option -> string *)
(* given some string option, return the string "Hello there ...", where ... is either the string if SOME or you if NONE *)
fun greeting (s: string option) =
	let val name = if isSome s then valOf s else "you"
	in
		"Hello there " ^ name
	end


(* int list * int list -> int list *)
(* given a list of integers and another list of nonnegative integers, repeats the integers in the first list according to the numbers indicated by the second list *)
(* Assume: the lists are the same length *)
fun repeat (xs: int list, ys: int list) = 
	if (null xs orelse null ys)
	then []
	else if (hd ys) > 0
		 then hd xs :: repeat (xs, ((hd ys)-1)::tl ys)
		 else repeat (tl xs, tl ys)


(* int option * int option -> int option *)
(* given two "optional" integers, adds them if they are both present (returning SOME of their sum), or returns NONE if at least one of the two arguments is NONE *)
fun addOpt (n1: int option, n2: int option) =
	if not (isSome n1) orelse not (isSome n2)
	then NONE
	else SOME (valOf n1 + valOf n2)


(* int option list -> int option *)
(* adds all integers in the list that are not NONE *)
fun addAllOpt (xs: int option list)	=
	let fun helper (xs: int option list, rsf: int option) =
			if null xs
			then rsf
			else 
				if isSome (hd xs)
				then if isSome rsf
					  then helper (tl xs, SOME (valOf (hd xs) + valOf rsf))
					  else helper (tl xs, hd xs)
				else
					helper (tl xs, rsf)
	in
		helper (xs, NONE)
	end
		

(* bool list -> bool *)
(* given a list of booleans returns true if there is at least one of them that is true, otherwise returns false. (If the list is empty it should return false because there is no true.) *)
fun any (xs: bool list) =
	if null xs then false else (hd xs orelse any (tl xs))

(* bool list -> bool *)
(* return true if all true in list otherwise false *)
fun all (xs: bool list) =
	if null xs then true else (hd xs andalso all (tl xs))


(* int list * int list -> int*int list *)
(* given two lists, create a list of consecutive pairs; stops when one of the lists is empty *)
fun zip (xs: int list, ys: int list) =
	if null xs orelse null ys
	then []
	else (hd xs, hd ys) :: zip (tl xs, tl ys)


(* int list * int list -> int*int list *)
(* given two lists, create a list of consecutive pairs; recycles from the start if one list is empty *)
fun zipRecycle (xs: int list, ys: int list) =
	if null xs orelse null ys
	then []
	else let fun zip_lists (tempxs: int list, tempys: int list, recycled: bool) =
				if (recycled andalso (null tempxs orelse null tempys)) orelse (null tempxs andalso null tempys)
				then []
				else 
					if null tempys
					then (hd tempxs, hd ys) :: zip_lists (tl tempxs, tl ys, true)
					else
						if null tempxs
						then (hd xs, hd tempys) :: zip_lists  (tl xs, tl tempys, true)
						else (hd tempxs, hd tempys) :: zip_lists  (tl tempxs, tl tempys, false)
		in
			zip_lists (xs, ys, false)
		end


(* int list * int list -> (int*int) list option *)
(*  return SOME of a list when the original lists have the same length, and NONE if they do not *)
fun zipOpt (xs: int list, ys: int list) =
	let fun length (xs: int list) =
		if null xs
		then 0
		else 1 + length (tl xs)
	in
		if length (xs) <> length (ys)
		then NONE
		else
			SOME (zip (xs, ys))
	end
		 

(* (string * int) list * string -> int option *)
(*  takes a list of pairs (s, i) and also a string s2 to look up. It then goes through the list of pairs looking for the string s2 in the first component. If it finds a match with corresponding number i, then it returns SOME i. If it does not, it returns NONE *)
fun lookup (pairs: (string * int) list, key: string) =
	if null (pairs)
	then NONE
	else if (#1 (hd pairs)) = key
		 then SOME (#2 (hd pairs))
		 else lookup (tl pairs, key)


(* int list -> int list * int list *)
(* given a list of integers creates two lists of integers, one containing the non-negative entries, the other containing the negative entries. Relative order is preserved: All non-negative entries appear in the same order in which they were on the original list, and similarly for the negative entries *)
fun splitup (xs: int list) =
	let fun pop_lists (xs: int list, poss: int list, negs: int list) =
			if null xs
			then (poss, negs)
			else
				if hd xs < 0
				then pop_lists (tl xs, poss, negs @ [hd xs])
				else pop_lists (tl xs, poss @ [hd xs], negs)
	in
		pop_lists (xs, [], [])
	end


(* int list * int -> int list * int list *)
(* splits up a list based on a given threshold *)
fun splitAt (xs: int list, threshold: int) =
	let fun populate_lists (xs: int list, lte: int list, gt: int list) =
		if null xs
		then (gt, lte)
		else
			if hd xs <= threshold
			then populate_lists (tl xs, lte @ [hd xs], gt)
			else populate_lists (tl xs, lte, gt @ [hd xs])
	in
		populate_lists (xs, [], [])
	end


(* int list -> boolean *)
(* produce true if given list is sorted in increasing order *)
fun isSorted (xs: int list) =
	if null xs orelse null (tl xs)
	then true
	else if hd xs <= hd (tl xs)
		 then isSorted(tl xs)
		 else false
	
(* int list -> boolean *)
(* produce true if given list is sorted in increasing or decreasing order *)
fun isAnySorted (xs: int list) =
	isSorted xs orelse isSorted (rev xs)

(* int list * int list -> int list *)
(* merges two lists of integers that are sorted from smallest to largest into one sorted list *)
fun sortedMerge (xs: int list, ys: int list) =
	if null xs andalso null ys
	then []
	else if null xs
		 then hd ys :: sortedMerge (xs, tl ys)
		 else if null ys
			  then hd xs :: sortedMerge (tl xs, ys)
		      else if hd xs <= hd ys
				   then hd xs :: sortedMerge (tl xs, ys)
				   else hd ys :: sortedMerge (xs, tl ys)

(* int list -> int list *)
(* sort the list in ascending order *)
fun qsort (xs: int list) =
	if null xs
	then []
	else if null (tl xs)
		 then xs
		 else let val pivot = hd xs
				  val (greater, less) = splitAt (tl xs, pivot)
			  in
				 qsort (less) @ [pivot] @ qsort (greater)
			  end


(* int list -> int list * int list *)
(* produce two lists by alternating the elements between the lists *)
fun divide (xs: int list) =
	let fun alt_split (xs: int list, lista: int list, listb: int list) =
			if null xs
			then (lista, listb)
			else if length xs = 1 then (lista @ [hd xs], listb)
			else alt_split (tl (tl xs), lista @ [hd xs], listb @ [hd (tl xs)])
	in
		alt_split (xs, [], [])
	end

(* int list -> int list *)
(* sorts a given list of integers using divide and sortedMerge *)
fun not_so_quick_sort (xs: int list) =
	if null xs orelse null (tl xs)
	then xs
	else let val (lista, listb) = divide xs
		 in
			 sortedMerge (not_so_quick_sort (lista), not_so_quick_sort (listb))
		 end


(* int * int -> int * int *)
(* given two numbers k and n it attempts to evenly divide k into n as many times as possible, and returns a pair (d, n2) where d is the number of times while n2 is the resulting n after all those divisions *)
fun fullDivide (k: int, n: int) =
	let fun divide (n: int, d: int) =
			if k = 0 orelse n mod k <> 0
			then (d, n)
			else divide (n div k, d+1)
	in
		divide (n, 0)
	end


(* int -> (int * int) list *)
(* returns the prime factorization of n as a list of pairs (prime, multiplicity) *)
fun factorize (n: int) =
	let fun h (n: int, divisor: int, rsf: (int * int) list) =
			if (divisor * divisor) > n
			then if n > 1 then rsf @ [(n,1)] else rsf
			else let val (d, n2) = fullDivide (divisor, n)
				 in
					 if d > 0
					 then h (n2, divisor+1, rsf @ [(divisor, d)])
					 else h (n, divisor+1, rsf)
				 end
	in
		if n < 2
		then []
		else h (n, 2, [])
	end


(* (int * int) list -> int *)
(* given a factoriaztion of a number n, computes back the number n *)
fun multiply (factors: (int * int) list) =
	let fun pow (base: int, exp: int) =
			if exp = 0
			then 1
			else base * pow (base, exp-1)
	in
		if null factors
		then 1
		else pow (#1 (hd factors), #2 (hd factors)) * multiply (tl factors)
	end


(* (int * int) list -> int list *)
(* given a factorization list result from factorize creates a list all of possible products produced from using some or all of those prime factors no more than the number of times they are available *)
fun all_products (factors: (int * int) list) =
	let fun pow (base: int, exp: int) =
			if exp = 0
			then 1
			else base * pow (base, exp-1)

		fun insert_unique (num: int, xs: int list) =
			if null xs
			then num :: xs
			else if num = hd xs then xs 
			else if num >= hd xs
				 then hd xs :: insert_unique (num, tl xs)
				 else num :: xs

		fun multiply (num: int, xs: int list) = 
			let fun helper (xs: int list, rsf: int list) =
					if null xs
					then rsf
					else helper (tl xs, insert_unique ((num * hd xs), rsf))
			in
				helper (xs, xs)
			end
			
		fun add_divisors_nonempty (factor: (int * int), divisors: int list) =
			if (#2 factor) < 1
			then divisors
			else add_divisors_nonempty ((#1 factor, (#2 factor)-1), multiply (pow (#1 factor, #2 factor), divisors))

		fun add_divisors (factor: (int * int), divisors: int list) =
			if (#2 factor) < 0
			then divisors
			else add_divisors ((#1 factor, (#2 factor)-1), pow (#1 factor, #2 factor) :: divisors)


	in
		let fun helper (factors: (int * int) list, divisors: int list) =
				if null factors
				then divisors
				else if null divisors
					 then helper (tl factors, add_divisors (hd factors, divisors))
					 else helper (tl factors, add_divisors_nonempty (hd factors, divisors))
		in
			if null factors
			then [1]
			else helper (factors, [])
		end
	end
		
