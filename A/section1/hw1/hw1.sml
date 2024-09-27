(* Date is: int*int*int *)
(* interp. a 3 part tuple, where the first int is the year, second int is the month, and third int is the day *)

(* Date Date -> bool *)
(* produce true if the first argument is a date that comes before the second argument *)
(* Assume: both given dates are valid *)
fun is_older (d1: int*int*int, d2: int*int*int) = 
	#1 d1 < #1 d2 orelse
	(#1 d1 = #1 d2 andalso #2 d1 < #2 d2) orelse 
	(#1 d1 = #1 d2 andalso #2 d1 = #2 d2 andalso #3 d1 < #3 d2)
	
	
(* (listof Date) int -> int *)
(* produce the number of dates in the (listof Date) in the given month (int) *)
fun number_in_month (lod: (int*int*int) list, month: int) =
	if null (lod)
	then 0
	else 
		if #2 (hd lod) = month
		then 1 + number_in_month (tl lod, month)
		else number_in_month (tl lod, month)

(* (listof Date) (listof int) -> int *)
(* produce the number of dates in the list of dates that are in any of the months in the list of months *)
(* Assume: the list of months has no numbers repeated *)
fun number_in_months (lod: (int*int*int) list, lom: int list) =
	if null (lom)
	then 0
	else number_in_month (lod, hd lom) + number_in_months (lod, tl lom)


(* (listof Date) int -> (listof Date) *)
(* produce a list of dates from the given list of dates argument in the given month *)
fun dates_in_month (lod: (int*int*int) list, month: int) =
	if null (lod)
	then []
	else 
		if #2 (hd lod) = month
		then hd lod :: dates_in_month (tl lod, month)
		else dates_in_month (tl lod, month)


(* (listof Date) (listof int) -> (listof Date) *)
(* produce a list of the dates in lod that are in any of the months in lom *)
(* Assume: the list of months has no numbers repeated *)
fun dates_in_months (lod: (int*int*int) list, lom: int list) =
	if null (lom)
	then []
	else dates_in_month (lod, hd lom) @ dates_in_months (lod, tl lom)
		

(* (listof string) int -> string *)
(* produce the nth string in a list of strings *)
(* Assume: los has enough elements, and n >= 1 *)
fun get_nth (los: string list, n: int) =
	if n = 1
	then hd los	
	else get_nth (tl los, n-1)


(* Date -> string *)
(* convert a date to a formatted string: "Month DD, YYYY" *)
fun date_to_string (date: int*int*int) =
	let val months = ["January","February","March","April","May","June","July",
	                  "August","September","October","November","December"]
		val day = #3 date
		val month = #2 date
		val year = #1 date	
	in
		get_nth (months, month) ^ " " ^ Int.toString (day) ^ ", " ^ Int.toString(year)
	end

(* int int list -> int *)
(*  return an int n such that the first n elements of the list add to less than sum, but the first
n + 1 elements of the list add to sum or more.*)
(* Assume: given int and int list contain positive numbers only *)
fun number_before_reaching_sum (sum: int, xs: int list) =
	let fun h (n: int, current_sum: int, xs: int list) =
		if null (xs)
		then n
		else if current_sum + hd xs < sum
			 then h (n+1, current_sum + hd xs, tl xs)
			 else n
	in
		h (0, 0, xs)
	end

(* int -> int *)
(* given a day of the year between 1 and 365, return month that day is in *)
fun what_month (doy: int) =
	let val days_per_month = [31,28,31,30,31,30,31,31,30,31,30,31]
	in
		1 + number_before_reaching_sum (doy, days_per_month) 
	end


(* int int -> int list *)
(*  return an int list
[m1,m2,...,mn] where m1 is the month of day1, m2 is the month of day1+1, ..., and mn is the month
of day day2 *)
fun month_range (day1: int, day2: int) =
	if day1 > day2
	then []
	else what_month (day1) :: month_range (day1+1, day2)


(* (listof Date) -> (int*int*int) option *)
(* produces SOME d, where d is the oldest date in the list, otherwise NONE *)
fun oldest (lod: (int*int*int) list) =
	if null (lod)
	then NONE
	else
		let fun oldest_nonempty (lod: (int*int*int) list, oldest: (int*int*int)) =
			if null (lod)
			then oldest
			else
				if is_older (hd lod, oldest)
				then oldest_nonempty (tl lod, hd lod)
				else oldest_nonempty (tl lod, oldest)
		in
			SOME (oldest_nonempty (tl lod, hd lod))
		end


(* helpers for challenge problems *)
fun member (item: int, xs: int list) =
	not (null xs) andalso (item = hd xs orelse member (item, tl xs))
		
fun remove_duplicates (xs: int list) =
	if null (xs)
	then []
	else if member (hd xs, tl xs)
		 then remove_duplicates (tl xs)
		 else hd xs :: remove_duplicates (tl xs)

(* (listof Date) (listof int) -> int *)
(* produce the number of dates in the list of dates that are in any of the months in the list of months *)
fun number_in_months_challenge (lod: (int*int*int) list, lom: int list) =
	number_in_months (lod, remove_duplicates (lom))

(* (listof Date) (listof int) -> (listof Date) *)
(* produce a list of the dates in lod that are in any of the months in lom *)
fun dates_in_months_challenge (lod: (int*int*int) list, lom: int list) =
	dates_in_months (lod, remove_duplicates (lom))


(* Date -> bool *)
(* determine if given date is a real date in the common era *)
fun reasonable_date (date: int*int*int) =
	let val year = #1 date
		val month = #2 date
		val day = #3 date

		val leap = (year mod 4) = 0 andalso (year mod 100) <> 0
		val feb_days = if leap then 29 else 28
		
		val days_per_month = [31,feb_days,31,30,31,30,31,31,30,31,30,31]
			
		fun get_nth (xs: int list, n: int) =
			if n = 1
			then hd xs
			else get_nth (tl xs, n-1)

		fun valid_month (month: int) =
			month > 0 andalso month < 13

		fun valid_day (year: int, month: int, day: int) =
			day <= get_nth (days_per_month, month)
			
	in
		year > 0 andalso valid_month (month) andalso valid_day (year, month, day)
	end
		
