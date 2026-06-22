(* demo.sml - B-tree (minimum degree t=3) walkthrough. Deterministic: identical
   output on every run and under both compilers. *)

structure B = Btree
val ic = Int.compare

fun intList xs = "[" ^ String.concatWith "," (List.map Int.toString xs) ^ "]"

val () = print ("B-tree minimum degree t = " ^ Int.toString B.minDegree
                ^ " (a node holds " ^ Int.toString (B.minDegree - 1) ^ ".."
                ^ Int.toString (2 * B.minDegree - 1) ^ " keys)\n\n")

val () = print "Inserting keys 1..5 (fits one leaf), then 6 (forces a split):\n"
val five = B.fromList ic (List.map (fn k => (k, k * k)) [1,2,3,4,5])
val () = print ("  after 1..5: height = " ^ Int.toString (B.height five)
                ^ ", size = " ^ Int.toString (B.size five) ^ "\n")
val six = B.insert ic five 6 36
val () = print ("  after +6:   height = " ^ Int.toString (B.height six)
                ^ ", size = " ^ Int.toString (B.size six) ^ "\n")
val () = print ("  keys        = " ^ intList (B.keys six) ^ "\n")

val () = print "\nBulk-loading a scrambled 1..100 (value = key*key):\n"
val perm = List.tabulate (100, fn i => (i * 37) mod 100 + 1)
val big = B.fromList ic (List.map (fn k => (k, k * k)) perm)
val () = print ("  size        = " ^ Int.toString (B.size big) ^ "\n")
val () = print ("  height      = " ^ Int.toString (B.height big) ^ "\n")
val () = print ("  find 42     = " ^ Int.toString (valOf (B.find ic big 42)) ^ "\n")
val () = print ("  member 100  = " ^ Bool.toString (B.member ic big 100) ^ "\n")
val () = print ("  member 101  = " ^ Bool.toString (B.member ic big 101) ^ "\n")
val () = print ("  range 10..15 keys = "
                ^ intList (List.map #1 (B.rangeQuery ic big 10 15)) ^ "\n")
val () = print ("  first 8 keys      = "
                ^ intList (List.take (B.keys big, 8)) ^ "\n")
