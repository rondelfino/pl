fun old_max (xs : int list) =
	if null xs
	then 0
	else if null (tl xs)
	then hd xs
	else
		let val tl_ans = old_max (tl xs)
		in
			if hd xs > tl_ans
			then hd xs
			else tl_ans
		end

(* tail recursive algorithm for finding max in an int list *)
fun old_max_tr (xs : int list) =
	let fun h (list : int list, current_max : int) =
			if null list
			then 
				current_max
			else if hd list > current_max
			then 
				h (tl list, hd list)
			else 
				h (tl list, current_max)
	in
		h (xs, 0)
	end

fun max_tr (xs : int list) =
	if null xs
	then NONE
	else
		let fun max_nonempty (list : int list, current_max : int) =
				if null list
				then
					current_max
				else if hd list > current_max
				then
					max_nonempty (tl list, hd list)
				else
					max_nonempty (tl list, current_max)
		in
			SOME (max_nonempty (xs, 0))
		end

fun max1 (xs : int list) =
	if null xs
	then NONE
	else
		let val tl_ans = max1 (tl xs)
		in
			if isSome tl_ans andalso valOf tl_ans > hd xs
			then tl_ans
			else SOME (hd xs)
		end
	
fun max2 (xs : int list) =
	if null xs
	then NONE
	else
		let fun max_nonempty (xs : int list) =
			if null (tl xs)
			then hd xs
			else 
				let val tl_ans = max_nonempty (tl xs)
				in
					if hd xs > tl_ans
					then hd xs
					else tl_ans
				end
		in
			SOME (max_nonempty xs)
		end

fun countup (from : int, to: int) = 
	if from = to
	then 
		to :: []
	else 
		from :: countup (from+1, to)
