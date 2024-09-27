(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* string * string list -> string option *)
(* return NONE if string is not in the list, otherwise return SOME list without the string *)
fun all_except_option (s, slist) =
    case slist of
         [] => NONE
      | x::xs' => if same_string (x,s) then SOME xs' else case all_except_option (s, xs') of
																NONE => NONE
															  | SOME rest => SOME (x::rest)


(* string list list * string -> string list *)
(* return the strings that are in some list in substitutions that also has s, but s itself is not included *)
(* ASSUME: No repeats in given subsitutions list *)
fun get_substitutions1 (subs, s) =
    case subs of
         [] => []
      | x::xs' => case all_except_option (s,x) of
						NONE => get_substitutions1 (xs',s)
					  | SOME list => list @ get_substitutions1 (xs',s)


(* string list list * string -> string list *)
(* tail recursive version of get_substitutions1 *)
fun get_substitutions2 (subs, s) =
	let fun helper (subs, rsf) =
			case subs of
				 [] => rsf
			   | x::xs' => case all_except_option (s,x) of
								NONE => helper (xs',rsf)
							  | SOME list => helper (xs',rsf@list)
	in
		helper (subs, [])
	end


(* string list list * {first:string,middle:string,last:string} -> {first:string,middle:string,last:string} list *)
(* return a list of names with the all possible names with first name substituted *)
fun similar_names (subs, name) =
	let val {first=f,middle=m,last=l} = name
		fun helper (subs, names) =
				case subs of
					 [] => names
				   | x::xs' => helper(xs', names@[{first=x,middle=m,last=l}])
	in
		helper (get_substitutions2(subs, f), [name])
	end



(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* card -> color *)
(* return color of given card *)
fun card_color card =
	case card of
		 (Hearts,_) => Red
	   | (Diamonds,_) => Red
	   | _ => Black


(* card -> int *)
(* return value of card *)
(* numbered cards have their number as value, aces are 11, face cards are 10 *)
fun card_value card = 
	case card of
		 (_,Num x) => x
	   | (_,Ace) => 11
	   | _ => 10


(* card list * card * exn -> card list *)
(* remove the given card from the given list *)
(* removes the card only once *)
(* if c is not in the list raise exn e *)
fun remove_card (cs, c, e) =
	case cs of
		 [] => raise e
	   | x::xs' => if c=x then xs' else x::remove_card (xs', c, e)


(* card list -> bool *)
(* return true if the cards in the list are all the same color *)
fun all_same_color cards =
	case cards of
		 [] => true
	   | first::(second::rest) => card_color first = card_color second andalso all_same_color (second::rest)
	   | _ => true


(* card list -> int *)
(* sum the card values in the given list *)
fun sum_cards cards =
	let fun helper (cards, sum) = 
			case cards of
				 [] => sum
			   | c::cs => helper (cs, sum + card_value c)
	in
		helper (cards, 0)
	end

(* card list * int -> int *)
(* compute the score given a player's held cards and the goal *)
fun score (cards, goal) =
	let val sum = sum_cards cards
		val prelim_score = if sum > goal then 3 * (sum-goal) else goal - sum
	in
		if all_same_color cards
		then prelim_score div 2
		else prelim_score
	end

(* card list * move list * int -> int *)
(* takes a card list (the card-list) a move list
(what the player “does” at each point), and an int (the goal) and returns the score at the end of the
game after processing (some or all of) the moves in the move list in order *)
fun officiate (cards, moves, goal) =
	let fun helper (cs, moves, held_cards, sum) =
			if sum > goal
			then score (held_cards,goal)
			else
			case (cs,moves) of
				 (_,[]) => score (held_cards,goal)
			   | ([],Draw::rest) => score (held_cards,goal)
			   | (c::cs',Draw::rest) => helper (cs', rest, c::held_cards, sum+card_value(c))
			   | (_,Discard (s,r)::rest) => let val new_hand = remove_card (held_cards, (s,r), IllegalMove)
											  in
													helper (cs, rest, new_hand, sum-card_value((s,r)))
											  end
	in
		helper (cards, moves, [], 0)
	end




fun min (x,y) = if x < y then x else y

(* card list * int -> int *)
(* similar to score but each ace can have a value of 1 or 11 and the lowest possible score is returned *)
fun score_challenge (cards, goal) =
	let fun score sum =
			let val prelim_score = if sum > goal then 3 * (sum-goal) else goal - sum
			in
				if all_same_color cards
				then prelim_score div 2
				else prelim_score
			end			

		fun min_score (cs,sum) =
			case cs of
				[] => score sum
			  | (_,Ace)::cs' =>  min (min_score (cs',sum+11), min_score (cs',sum+1)) 
			  | c::cs' => min_score (cs',sum+(card_value c))
			
	in
		min_score (cards,0)
	end


(* card list * move list * int -> int *)
(* similar to officiate but each ace can have a value of 1 or 11 and the lowest possible score is returned *)
fun officiate_challenge (cards, moves, goal) =
	let fun helper (cs, moves, held_cards, sum) =
			if sum > goal
			then score_challenge (held_cards,goal)
			else
			case (cs,moves) of
				 (_,[]) => score_challenge (held_cards,goal)
			   | ([],Draw::rest) => score_challenge (held_cards,goal)
			   | ((s,Ace)::cs',Draw::rest) => helper (cs', rest, (s,Ace)::held_cards, min (sum+11, sum+1))
			   | (_,Discard (s,Ace)::rest) => let val new_hand = remove_card (held_cards, (s,Ace), IllegalMove)
											  in
													helper (cs, rest, new_hand, min (sum-11,sum-1))
											  end
			   | (c::cs',Draw::rest) => helper (cs', rest, c::held_cards, sum+card_value(c))
			   | (_,Discard (s,r)::rest) => let val new_hand = remove_card (held_cards, (s,r), IllegalMove)
											  in
													helper (cs, rest, new_hand, sum-card_value((s,r)))
											  end
	in
		helper (cards, moves, [], 0)
	end


(* card list * int -> move list *)
(* returns a move-list such that calling
officiate with the card-list, the goal, and the move-list has this behavior:
	• The value of the held cards never exceeds the goal.
	• A card is drawn whenever the goal is more than 10 greater than the value of the held cards. As a
	detail, you should (attempt to) draw, even if no cards remain in the card-list.
	• If a score of 0 is reached, there must be no more moves.
	• If it is possible to reach a score of 0 by discarding a card followed by drawing a card, then this
	must be done. Note careful_player will have to look ahead to the next card, which in many card
	games is considered “cheating.” Also note that the previous requirement takes precedence: There
	must be no more moves after a score of 0 is reached even if there is another way to get back to 0. *)

(*  if score is 0 return moves
	if the goal - sum (held_cards) > 10 then Draw else check if a discard and draw reduces score, if it does do it
																								  else end the game 
	if a discard and draw reduces score to 0 do it
	*)
fun careful_player (cards, goal) =
		(* return (card list * move list * score) with updated hand and move list if score was reduced *)
		(* return the min score we can achieve by discarding a held card and drawing the given card *)
	let fun discard_and_draw (held_cards, moves, card, s) =
			let fun helper (hand, new_hand, new_moves, min_score) =
					case hand of
						 [] => (new_hand, moves@new_moves, min_score)
					   | c::cs => let val current_hand = card :: remove_card (held_cards, c, IllegalMove) 
									  val new_score = score (current_hand, goal)
								  in
										if new_score < min_score then helper (cs, current_hand, [Discard(c),Draw], new_score)
										else helper (cs, new_hand, new_moves, min_score) 
								  end
			in
				helper (held_cards, held_cards, [], s)
			end
			
		fun helper (cards, held_cards, moves, s) =
			let val hand_value = sum_cards held_cards
			in
				case (cards, s, (goal-hand_value)>10) of
					 (_,0,_) => moves
				   | ([],_,_) => moves
				   | (c::cs',_,true) => let val new_hand = c::held_cards in helper (cs', new_hand, Draw::moves, score (new_hand, goal)) end
				   | (c::cs',_,false) => let val (new_hand, updated_moves, new_score) = discard_and_draw (held_cards, moves, c, s)										 in
										 	 if new_score < s then helper (cs', new_hand, updated_moves, new_score)
											 else moves (* no valid moves that can reduce score *)
										 end
			end

		in
			helper (cards, [], [], score ([], goal))
		end
