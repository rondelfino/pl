(* ('b -> 'c option) -> ('a -> 'b option) -> 'a -> 'c option *)
(* compose two functions with optional values. If either function returns NONE, then the result is NONE *)
fun compose_opt f g x =
	case g x of
		 NONE => NONE
	   | SOME y => f y


(* ('a -> 'a) -> ('a -> bool) -> 'a -> 'a *)
(* do_until f p x will apply f to x and f again to that result and so on until p x is false.*)
fun do_until f p x =
	case p x of
		 false => x
	   | true => do_until f p (f x) 

(* int -> int *)
(* compute factorial n *)
fun factorial n =
	#1 (do_until (fn (x, n) => (x * n, n - 1)) (fn (_, n) => n > 1) (1, n))


(* (''a -> ''a) -> ''a -> ''a *)
fun fixed_point f x =
	do_until f (fn x => f x <> x) x


(* ('a -> 'b) -> 'a * 'a -> 'b * 'b *)
(* given a function that takes 'a values to 'b values and a pair of 'a values returns the corresponding pair of 'b values *)
fun map2 f (x, y) = (f x, f y)

(* ('b -> 'c list) -> ('a -> 'b list) -> 'a -> 'c list *)
(* app_all f g x will apply f to every element of the list g x and concatenate the results into a single list *)
fun app_all f g x = List.concat (List.map f (g x))
		

fun foldr f acc xs =
	case xs of
		 [] => acc
	   | (x::xs') => f (x, (foldr f acc xs'))

(* ('a -> bool) -> 'a list -> 'a list * 'a list *)
(* the first part of the result contains the second argument elements for which the first element evaluates to true and the second part of the result contains the other second argument elements.  Traverse the second argument only once  *)
fun partition pred xs =
	let fun helper (a, b) xs =
			case xs of
				 [] => (a, b)
			   | (x::xs') => case pred x of
								  false => helper ((a, b @ [x])) xs'
								| true => helper ((a @ [x], b)) xs'
	in
		helper ([], []) xs
	end
	
	
(* ('a -> bool) -> 'a list -> 'a list * 'a list *)
fun partition pred xs =
    let
        (* The helper function uses two difference lists *)
        fun helper (true_acc, false_acc) [] = (true_acc [], false_acc [])
          | helper (true_acc, false_acc) (x::xs) =
              if pred x then
                  helper (fn ys => true_acc (x::ys), false_acc) xs  (* Add to true partition *)
              else
                  helper (true_acc, fn ys => false_acc (x::ys)) xs  (* Add to false partition *)
    in
        helper (fn ys => ys, fn ys => ys) xs  (* Initial accumulators as identity functions *)
    end


(* (’a -> (’b * ’a) option) -> ’a -> ’b list *)
fun unfold f n =
	case f n of
		 NONE => []
	   | SOME(a, b) => a :: (unfold f b)


fun fact n = foldr op* 1 (unfold (fn n => if n = 0 then NONE else SOME(n, n-1)) n)

fun map f xs = List.foldr (fn (x, acc) => (f x) :: acc) [] xs

fun filter pred xs = List.foldr (fn (x, acc) => if pred x then x :: acc else acc) [] xs

(* fun foldl f acc xs = *)
	(* foldr (fn (g, h) => g o h) (fn x => x) xs *)

datatype 'a node =
    Leaf                          
  | Node of 'a * 'a node * 'a node 

fun map_tree f head = 
	case head of
		 Leaf => Leaf
	   | Node (x, l, r) => Node (f x, map_tree f l, map_tree f r)

fun fold_tree f acc head =
	case head of
		 Leaf => Leaf
	   | Node (x, l, r) => let val left_res = fold_tree f acc l
							   val right_res = fold_tree f acc r
						   in
							   f (x, left_res, right_res)
						   end

fun filter_tree pred node =
	case node of
		 Leaf => Leaf
	   | Node (x, l, r) => case pred x of
								false => Leaf
							  | true => Node (x, filter_tree pred l, filter_tree pred r)
		 
	
	
