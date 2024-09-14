fun good_max (xs : int list) =
	if null xs
	then 0
	else if null (tl xs)
	then hd xs
	else
		let val tl_ans = good_max(tl xs)
		in
			if hd xs > tl_ans
			then hd xs
			else tl_ans
		end

fun good_max_tr (xs : int list) =
	let fun h (list : int list, current_max : int) =
			if null list
			then 
				current_max
			else if hd list > current_max
			then 
				h(tl list, hd list)
			else 
				h(tl list, current_max)
	in
		h(xs, 0)
	end

fun countup(from : int, to: int) = 
	if from = to
	then 
		to :: []
	else 
		from :: countup(from+1, to)
