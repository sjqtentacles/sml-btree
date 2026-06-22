(* Tests for sml-btree: persistent B-tree of minimum degree t=3.

   B-tree invariants are checked indirectly: every key inserted is retrievable,
   `toList` enumerates in ascending order, `size` tracks bindings, and `height`
   stays logarithmic (so node splits are happening). Cross-checked against plain
   sorted lists built from a scrambled permutation of 1..200. *)

structure Tests =
struct
  open Harness

  structure B = Btree
  val ic = Int.compare
  val sc = String.compare

  (* scrambled permutation of 1..n via i |-> ((i*73) mod p)+1-ish; use a prime
     stride coprime to n. For n=200 use stride 73 over modulus 200? Not bijective
     unless gcd=1. gcd(73,200)=1, so i*73 mod 200 is a bijection on Z/200. *)
  fun perm n stride = List.tabulate (n, fn i => (i * stride) mod n + 1)
  val perm200 = perm 200 73
  val sorted200 = List.tabulate (200, fn i => i + 1)

  fun isSorted ic xs =
    case xs of
        [] => true
      | [_] => true
      | a :: (rest as b :: _) => (ic (a, b) = LESS) andalso isSorted ic rest

  fun runAll () =
    let
      val () = section "Empty / make"
      val e : (int, int) B.tree = B.empty
      val () = checkBool "isEmpty empty" (true, B.isEmpty e)
      val () = checkInt "size empty" (0, B.size e)
      val () = checkInt "height empty" (0, B.height e)
      val () = checkIntList "keys empty" ([], B.keys e)
      val () = checkBool "find in empty" (false, B.member ic e 1)
      val () = checkInt "minDegree" (3, B.minDegree)

      val () = section "Small insert / find / overwrite"
      val t0 = B.insert ic e 5 50
      val t1 = B.insert ic t0 3 30
      val t2 = B.insert ic t1 7 70
      val () = checkInt "size 3" (3, B.size t2)
      val () = checkInt "height single node" (1, B.height t2)
      val () = checkIntList "keys sorted" ([3,5,7], B.keys t2)
      val () = checkIntList "values by key" ([30,50,70], B.values t2)
      val () = checkInt "find 5" (50, valOf (B.find ic t2 5))
      val () = checkBool "find absent" (false, Option.isSome (B.find ic t2 4))
      val t2b = B.insert ic t2 5 555
      val () = checkInt "overwrite value" (555, valOf (B.find ic t2b 5))
      val () = checkInt "overwrite keeps size" (3, B.size t2b)
      val () = checkInt "persistent original" (50, valOf (B.find ic t2 5))

      val () = section "Node split (insert > 2t-1 keys)"
      (* t=3 -> a leaf holds at most 5 keys; the 6th insert splits the root *)
      val five = B.fromList ic (List.map (fn k => (k, k)) [1,2,3,4,5])
      val () = checkInt "5 keys: still height 1" (1, B.height five)
      val six = B.insert ic five 6 6
      val () = checkInt "6th key forces split -> height 2" (2, B.height six)
      val () = checkInt "size after split" (6, B.size six)
      val () = checkIntList "split keeps order" ([1,2,3,4,5,6], B.keys six)
      val () = checkBool "all retrievable after split"
                 (true, List.all (fn k => B.member ic six k) [1,2,3,4,5,6])

      val () = section "Bulk insert (scrambled 1..200)"
      val big = B.fromList ic (List.map (fn k => (k, k * 2)) perm200)
      val () = checkInt "size 200" (200, B.size big)
      val () = checkIntList "keys sorted 1..200" (sorted200, B.keys big)
      val () = checkBool "keys strictly ascending" (true, isSorted ic (B.keys big))
      val () = checkBool "values follow keys (2k)"
                 (true, B.values big = List.map (fn k => k * 2) sorted200)
      val () = checkBool "every key retrievable"
                 (true, List.all (fn k => B.find ic big k = SOME (k*2)) sorted200)
      val () = checkBool "absent below" (false, B.member ic big 0)
      val () = checkBool "absent above" (false, B.member ic big 201)
      (* height is logarithmic: for n=200, t=3 it sits well under 8 *)
      val () = checkBool "height logarithmic (2..6)"
                 (true, B.height big >= 2 andalso B.height big <= 6)

      val () = section "Insertion order independence"
      val ascend = B.fromList ic (List.map (fn k => (k, k)) sorted200)
      val descend = B.fromList ic (List.map (fn k => (k, k)) (List.rev sorted200))
      val () = checkIntList "ascending build sorts" (sorted200, B.keys ascend)
      val () = checkIntList "descending build sorts" (sorted200, B.keys descend)
      val () = checkInt "ascending size" (200, B.size ascend)
      val () = checkInt "descending size" (200, B.size descend)

      val () = section "Duplicate keys collapse"
      val dups = B.fromList ic (List.map (fn k => (k, k)) [4,4,4,2,2,7,7,7,7,1])
      val () = checkInt "dedup size" (4, B.size dups)
      val () = checkIntList "dedup keys" ([1,2,4,7], B.keys dups)
      val dupsLast = B.fromList ic [(3,1),(3,2),(3,3)]
      val () = checkInt "last write wins" (3, valOf (B.find ic dupsLast 3))

      val () = section "rangeQuery"
      val () = checkIntList "range [50..60] keys"
                 ([50,51,52,53,54,55,56,57,58,59,60],
                  List.map #1 (B.rangeQuery ic big 50 60))
      val () = checkBool "range values are 2k"
                 (true, List.all (fn (k, v) => v = k * 2) (B.rangeQuery ic big 50 60))
      val () = checkInt "range [1..200] = all" (200, List.length (B.rangeQuery ic big 1 200))
      val () = checkIntList "empty range hi<lo" ([], List.map #1 (B.rangeQuery ic big 60 50))
      val () = checkIntList "range single" ([100], List.map #1 (B.rangeQuery ic big 100 100))

      val () = section "String-keyed B-tree"
      val sw = B.fromList sc (List.map (fn w => (w, String.size w))
                 ["delta","alpha","echo","bravo","charlie","alpha","foxtrot"])
      val () = checkInt "string size (alpha deduped)" (6, B.size sw)
      val () = checkStringList "string keys sorted"
                 (["alpha","bravo","charlie","delta","echo","foxtrot"], B.keys sw)
      val () = checkInt "string find" (7, valOf (B.find sc sw "charlie"))
      val () = checkBool "string absent" (false, B.member sc sw "golf")
    in
      Harness.run ()
    end

  val run = runAll
end
