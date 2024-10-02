use "ep2.sml";

val pof1 = pass_or_fail {grade = SOME 75, id = 1} = pass
val pof2 = pass_or_fail {grade = SOME 25, id = 1} = fail

val hp1 = has_passed {grade = SOME 75, id = 1} = true
val hp2 = has_passed {grade = SOME 74, id = 1} = false

val nm1 = number_misgraded ([]) = 0
val nm2 = number_misgraded ([(pass,{grade = SOME 74, id = 1}),(pass,{grade = SOME 75, id = 1})]) = 1

val th1 = tree_height (leaf) = 0
val th2 = tree_height (node {value = 5, left = leaf, right = leaf}) = 1
val th3 = tree_height (node { value = 5,
							 left = node { value = 3, left = leaf, right = leaf },
							 right = node { value = 4, left = leaf, right = leaf } 
							}) = 2
							
val tree4 = node { value = 7, 
                   left = node { value = 3, left = leaf, right = leaf }, 
                   right = node { value = 9, 
                                  left = leaf, 
                                  right = node { value = 12, left = leaf, right = leaf } 
                                } 
                 }
val th4 = tree_height tree4 = 3


val st1 = sum_tree (leaf) = 0
val st2 = sum_tree (node {value = 5, left = leaf, right = leaf}) = 5 
val st3 = sum_tree (node { value = 5,
							 left = node { value = 3, left = leaf, right = leaf },
							 right = node { value = 4, left = leaf, right = leaf } 
							}) = 12
		
val g1 = gardener (leaf) = leaf
val g2 = gardener (node { value = prune_me, left = leaf, right = leaf }) = leaf
val g3 = gardener (node { value = leave_me_alone,
							 left = node { value = prune_me, left = leaf, right = leaf },
							 right = node { value = leave_me_alone, left = leaf, right = leaf } 
							}) = (node { value = leave_me_alone,
										 left = leaf,
										 right = node { value = leave_me_alone, left = leaf, right = leaf } 
										})

val l1 = last [] handle Empty => "List is empty"
val l2 = last [1] = 1
val l3 = last [1,2,3,4,5] = 5

val t2 = take ([] : int list, 0) = ([] : int list)
val t3 = take ([1,2,3,4,5], 3) = [1,2,3]

val d1 = drop ([] : int list, 0) = ([] : int list)
val d2 = drop ([1,2,3,4,5], 3) = [4,5]

val c1 = (concat [] : int list) = ([] : int list)
val c2 = concat ([[1, 2], [3, 4], [5]]) = [1,2,3,4,5]
val c3 = concat [[true], [], [false, true]] = [true,false,true]

val go1 = getOpt (SOME 1, 2) = 1
val go2 = getOpt (NONE, 2) = 2

val j1 = join (SOME(SOME(1))) = SOME(1)
val j2 = (join (SOME(NONE)) : int option) = (NONE : int option)
val j3 = (join NONE : int option) = (NONE : int option)

val ip1 = is_positive ZERO = false
val ip2 = is_positive (SUCC ZERO) = true

(* val pred1 = pred ZERO handle Negative => "Negative predecessor" *)
val pred2 = pred (SUCC ZERO) = ZERO
val pred3 = pred (SUCC(SUCC ZERO)) = SUCC ZERO

val nti1 = nat_to_int ZERO = 0
val nti2 = nat_to_int (SUCC ZERO) = 1
val nti3 = nat_to_int (SUCC(SUCC ZERO)) = 2

val itn1 = int_to_nat 0 = ZERO
val itn2 = int_to_nat 3 = SUCC(SUCC(SUCC ZERO))

val a1 = add (ZERO, ZERO) = ZERO
val a2 = add (ZERO, (SUCC ZERO)) = SUCC ZERO
val a3 = add ((SUCC ZERO), (SUCC(SUCC(SUCC ZERO)))) = (SUCC(SUCC(SUCC(SUCC ZERO))))

val s1 = sub (ZERO, ZERO) = ZERO
val s2 = sub ((SUCC ZERO), ZERO) = SUCC ZERO
val s3 = sub ((SUCC(SUCC(SUCC ZERO))), (SUCC(SUCC ZERO))) = SUCC ZERO

val m1 = mult (ZERO, (SUCC ZERO)) = ZERO
val m2 = mult ((SUCC ZERO), ZERO) = ZERO
val m3 = mult ((SUCC ZERO), (SUCC(SUCC ZERO))) = (SUCC(SUCC ZERO))
val m4 = mult ((SUCC(SUCC ZERO)), (SUCC(SUCC(SUCC ZERO)))) = (SUCC(SUCC(SUCC(SUCC(SUCC(SUCC ZERO)))))) 

val lt1 = less_than (ZERO, (SUCC ZERO)) = true
val lt2 = less_than (ZERO, ZERO) = false
val lt3 = less_than ((SUCC ZERO), ZERO) = false

val ie1 = isEmpty (Elems []) = true
val ie2 = isEmpty (Elems [1]) = false
val ie3 = isEmpty (Range { from = 3, to = 1}) = true
val ie4 = isEmpty (Range { from = 3, to = 3}) = false
val ie5 = isEmpty (Union(Elems [], Elems[])) = true
val ie6 = isEmpty (Union(Elems [1], Elems [])) = false
val ie7 = isEmpty (Intersection(Elems [], Elems [1])) = true
val ie8 = isEmpty (Intersection(Elems [1], Elems [1])) = false
val ie9 = isEmpty (Intersection(Elems [1,2,3], Elems [4,5,6])) = true

val contains1 = contains (Elems [1,2,3], 3) = true
val contains2 = contains (Elems [], 0) = false
val contains3 = contains (Range { from=0, to=5 }, 5) = true
val contains4 = contains (Union (Elems [1,2,3], Elems [1,4,5]), 6) = false
val contains5 = contains (Intersection (Elems [1,2,3], Elems [4,5,6]), 5) = false

val tl1 = toList (Elems []) = []
val tl2 = toList (Elems [1,1,2,3]) = [1,2,3]
val tl3 = toList (Range { from = 1, to = 1}) = [1]
val tl4 = toList (Range { from = 1, to = 3}) = [1,2,3]
val tl5 = toList (Union((Elems [1,2,3]), (Elems [1,4,5]))) = [1,2,3,4,5]
val tl6 = toList (Intersection(Elems [], Elems [1])) = []
val tl7 = toList (Intersection(Elems [1,2,3], Elems [1])) = [1]
val tl8 = toList (Intersection(Elems [1,2,3,1,4,5], Elems [1,3,7])) = [1,3]
val tl9 = toList (Intersection((Range {from = 1, to = 10000}), Elems [1,3,7])) = [1,3,7]
val tl10 = toList (Intersection(Elems [1,2,3], Elems [4,5,6])) = []
