(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(* ((string * string * typ) list) * (pattern list) -> typ option *)
(* The first argument contains elements that look like ("foo","bar",IntT), which means constructor foo
makes a value of type Datatype "bar" given a value of type IntT. Assume list elements all have different
first fields (the constructor name), but there are probably elements with the same second field (the datatype
name). Under the assumptions this list provides, you “type-check” the pattern list to see if there exists = false
some typ (call it t) that all the patterns in the list can have. If so, return SOME t, else return NONE.
You must return the “most lenient” type that all the patterns can have. For example, given patterns
TupleP[Variable("x"),Variable("y")] and TupleP[Wildcard,Wildcard], return TupleT[Anything,Anything]
even though they could both have type TupleT[IntT,IntT]. As another example, if the only patterns
are TupleP[Wildcard,Wildcard] and TupleP[Wildcard,TupleP[Wildcard,Wildcard]], you must return
TupleT[Anything,TupleT[Anything,Anything]]. *)

fun typecheck_patterns (env, patterns) =
	let fun compare_types t1 t2 =
	  case (t1, t2) of
	       (Anything, _) => true
	     | (_, Anything) => true
	     | (TupleT ts1, TupleT ts2) => List.length ts1 = List.length ts2
	                                    andalso List.all (fn (a, b) => compare_types a b) (ListPair.zip(ts1, ts2))
	     | _ => t1 = t2

												
	fun typecheck_pattern env pat =
	  case pat of
	       Wildcard => SOME Anything
	     | Variable _ => SOME Anything
	     | UnitP => SOME UnitT
	     | ConstP _ => SOME IntT
	     | TupleP ps =>
	         let val types = List.map (typecheck_pattern env) ps
	         in
	             if List.all (fn x => x <> NONE) types
	             then SOME (TupleT (List.map valOf types))
	             else NONE
	         end
	     | ConstructorP (s, p) =>
	         let fun find_in_env e =
				 case e of
					  [] => NONE
					| ((cons, dt, expected_type)::env') =>
				                   if s = cons
				                   then case typecheck_pattern env p of
				                            SOME t => if compare_types t expected_type
				                                      then SOME (Datatype dt)
				                                      else NONE
				                          | NONE => NONE
				                   else find_in_env env'
	         in
	             find_in_env env
	         end	

	(* assume types are compatible *)
	fun combine_types t1 t2 =
		case (t1, t2) of
			 (Anything, t) => t
		   | (t, Anything) => t
		   | (TupleT t1, TupleT t2) => let val type_pairs = ListPair.zip (t1,t2) 
									   in  
										  (TupleT (List.map (fn (t1,t2) => combine_types t1 t2) type_pairs)) 
									   end
		   | (TupleT t1, t2) => TupleT t1
		   | (t1, TupleT t2) => TupleT t2
		   | _ => t1
	

		fun check_compatibility types =
			case types of
				 [] => true
			   | [t] => true
			   | (t1::t2::rest) => compare_types t1 t2 andalso check_compatibility (t2::rest)

		fun combine_helper res types =
			case types of
				 [] => res
			   | (t::t') => combine_helper (combine_types res t) t'
			
		val opt_types = (List.map (typecheck_pattern env) patterns)
	in
		case List.all (fn x => x <> NONE) opt_types of
			 false => NONE
		    | _ => let val types = List.map valOf opt_types
					   val compat = check_compatibility types
					in
						if compat then SOME (combine_helper Anything types) else NONE
					end
	end
						   
		 


(**** you can put all your code here ****)

(* string list -> string list *)
(* return only strings with the first letter capitalized *)
fun only_capitals strings =
	List.filter (fn s => Char.isUpper (String.sub(s, 0))) strings 


(* string list -> string *)
(* return the longest string in the list *)
fun longest_string1 strings =
	List.foldl (fn (s, acc) => if String.size s > String.size acc then s else acc) "" strings
	
(* string list -> string *)
(* return the longest string in the list *)
fun longest_string2 strings =
	List.foldl (fn (s, acc) => if String.size s >= String.size acc then s else acc) "" strings


(* int * int -> bool -> string list -> string *)
fun longest_string_helper f =
	List.foldl (fn (s, acc) => if f (String.size s, String.size acc) then s else acc) "" 
	
val longest_string3 = longest_string_helper op>
val longest_string4 = longest_string_helper op>=

(* string list -> string *)
(* return longest string that begins with an uppercase letter; "" if none*)
val longest_capitalized = longest_string3 o only_capitals

(* string -> string *)
(* reverse a string *)
val rev_string = String.implode o rev o String.explode


(* ('a -> 'b option) -> 'a list -> 'b *)
(* The first argument should be applied to elements of the second argument in order
until the first time it returns SOME v for some v and then v is the result of the call to first_answer.
If the first argument returns NONE for all list elements, then first_answer should raise the exception
NoAnswer. *)
fun first_answer f xs =
	case xs of
		 [] => raise NoAnswer
	   | x::xs' => case f x of
						NONE => first_answer f xs'
					  | SOME v => v
	
(* ('a -> 'b list option) -> 'a list -> 'b list option *)
(* The first argument should be applied to elements of the second
argument. If it returns NONE for any element, then the result for all_answers is NONE. Else the
calls to the first argument will have produced SOME lst1, SOME lst2, ... SOME lstn and the result of
all_answers is SOME lst where lst is lst1, lst2, ..., lstn appended together (order doesn’t matter). *)
fun all_answers f xs =
	let fun helper (xs, rsf) =
			case xs of
				 [] => SOME rsf
			   | x::xs' => case f x of
								NONE => NONE
							  | SOME lst => helper (xs', rsf @ lst)
	in
		helper (xs, [])
	end


(* pattern -> int *)
(* count number of wildcards *)
fun count_wildcards p =
	g (fn () => 1) (fn _ => 0) p

(* pattern -> int *)
(* returns the number of Wildcard patterns it contains plus the sum of the string lengths of all the variables
in the variable patterns it contains. *)
fun count_wild_and_variable_lengths p =
	g (fn () => 1) (fn s => String.size s) p

(* string * pattern -> int *)
(* returns the number of times the string appears as a variable in the pattern *)
fun count_some_var (s, p) =
	g (fn _ => 0) (fn x => if s=x then 1 else 0) p

(* pattern -> bool *)
(* return true if and only if all the variables appearing 
in the pattern are distinct from each other (i.e., use different strings) *)
fun check_pat p =
  let fun helper (p, acc) =
      case p of
           Variable s => s::acc
         | TupleP ps => List.foldl (fn (p, acc) => helper (p, acc)) acc ps
         | ConstructorP(_, p) => helper (p, acc)
         | _ => acc
  in
    let val strings = helper (p, [])
		fun unique strings =
			case strings of
				 [] => true
			   | (s::ss) => not (List.exists (fn x => x = s) ss) andalso unique ss
	in
		unique strings
	end
  end


(* valu * pattern -> (string * value) list option *)
(* return NONE if the pattern does not match and SOME lst where lst is the list of bindings if it does *)
fun match (v, p) =
	let fun helper (v, p, rsf) =
		case (v, p) of
			 (_, Wildcard) => SOME []
		   | (v, Variable s) => SOME (rsf @ [(s, v)])
		   | (Unit, UnitP) => SOME []
		   | (Const x, ConstP y) => if x=y then SOME [] else NONE
		   | (Tuple vs, TupleP ps) => if List.length vs = List.length ps then all_answers (fn vp_pair => let val (v, p) = vp_pair                                       in match (v, p) end) (ListPair.zip (vs, ps)) else NONE
		   | (Constructor(s1, v), ConstructorP(s2, p)) => if s1=s2 then match (v, p) else NONE
		   | _ => NONE
	in
		helper (v, p, [])
	end

(* valu -> pattern list -> (string * valu) list option *)
(* return NONE if no pattern in the list matches or SOME lst where
lst is the list of bindings for the first pattern in the list that matches *)
fun first_match v ps =
    SOME (first_answer (fn p => match (v, p)) ps) handle NoAnswer => NONE
	
