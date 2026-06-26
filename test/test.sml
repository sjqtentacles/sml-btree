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

      val () = section "insertWith / adjust / update"
      val acc0 = B.empty : (string, int) B.tree
      val acc1 = B.insertWith sc op+ acc0 "a" 1
      val acc2 = B.insertWith sc op+ acc1 "a" 10
      val acc3 = B.insertWith sc op+ acc2 "b" 5
      val () = checkInt "insertWith combines (1+10)" (11, valOf (B.find sc acc3 "a"))
      val () = checkInt "insertWith fresh key" (5, valOf (B.find sc acc3 "b"))
      val () = checkInt "insertWith size" (2, B.size acc3)
      val adj = B.adjust ic (fn v => v + 1) big 100
      val () = checkInt "adjust present" (201, valOf (B.find ic adj 100))
      val () = checkInt "adjust missing no-op size" (200, B.size (B.adjust ic (fn v => v) big 999))
      val upIns = B.update ic (fn _ => SOME 2500) big 250
      val () = checkInt "update inserts" (2500, valOf (B.find ic upIns 250))
      val () = checkInt "update insert size" (201, B.size upIns)
      val upDel = B.update ic (fn _ => NONE) big 100
      val () = checkBool "update NONE deletes" (false, B.member ic upDel 100)
      val () = checkInt "update delete size" (199, B.size upDel)
      val upMod = B.update ic (fn SOME v => SOME (v + 5) | NONE => NONE) big 100
      val () = checkInt "update modifies" (205, valOf (B.find ic upMod 100))

      val () = section "folds / app / mapValues / filter"
      val sumKeys = B.foldl (fn (k, _, a) => a + k) 0 big
      val () = checkInt "foldl sum keys 1..200" (200 * 201 div 2, sumKeys)
      val () = checkBool "foldl ascending order"
                 (true, isSorted ic (List.rev (B.foldl (fn (k,_,a) => k :: a) [] big)))
      val () = checkBool "foldr descending accumulation"
                 (true, isSorted ic (B.foldr (fn (k,_,a) => k :: a) [] big))
      val cnt = ref 0
      val () = B.app (fn _ => cnt := !cnt + 1) big
      val () = checkInt "app visits all" (200, !cnt)
      val mv = B.mapValues (fn v => v + 1) big
      val () = checkInt "mapValues" (101, valOf (B.find ic mv 50))
      val () = checkIntList "mapValues keeps keys" (sorted200, B.keys mv)
      val evens = B.filter ic (fn (k, _) => k mod 2 = 0) big
      val () = checkInt "filter size (evens 1..200)" (100, B.size evens)
      val () = checkBool "filter content" (true, List.all (fn k => k mod 2 = 0) (B.keys evens))

      val () = section "min / max / floor / ceiling / pred / succ"
      val () = checkInt "min key" (1, #1 (valOf (B.min big)))
      val () = checkInt "max key" (200, #1 (valOf (B.max big)))
      val () = checkBool "min empty NONE" (true, B.min e = NONE)
      val () = checkBool "max empty NONE" (true, B.max e = NONE)
      val () = checkInt "floor 100 = 100" (100, #1 (valOf (B.floor ic big 100)))
      val () = checkInt "ceiling 100 = 100" (100, #1 (valOf (B.ceiling ic big 100)))
      val sparse = B.fromList ic (List.map (fn k => (k, k)) [10,20,30,40,50])
      val () = checkInt "floor 25 = 20" (20, #1 (valOf (B.floor ic sparse 25)))
      val () = checkInt "ceiling 25 = 30" (30, #1 (valOf (B.ceiling ic sparse 25)))
      val () = checkBool "floor below min NONE" (true, B.floor ic sparse 5 = NONE)
      val () = checkBool "ceiling above max NONE" (true, B.ceiling ic sparse 99 = NONE)
      val () = checkInt "predecessor 30 = 20" (20, #1 (valOf (B.predecessor ic sparse 30)))
      val () = checkInt "successor 30 = 40" (40, #1 (valOf (B.successor ic sparse 30)))
      val () = checkBool "predecessor of min NONE" (true, B.predecessor ic sparse 10 = NONE)
      val () = checkBool "successor of max NONE" (true, B.successor ic sparse 50 = NONE)

      val () = section "rangeFold"
      val rfSum = B.rangeFold ic (fn (k, _, a) => a + k) 0 big 50 60
      val () = checkInt "rangeFold sum [50..60]" (List.foldl op+ 0 [50,51,52,53,54,55,56,57,58,59,60], rfSum)
      val () = checkIntList "rangeFold keys match rangeQuery"
                 (List.map #1 (B.rangeQuery ic big 50 60),
                  List.rev (B.rangeFold ic (fn (k,_,a) => k :: a) [] big 50 60))
      val () = checkInt "rangeFold empty hi<lo" (0, B.rangeFold ic (fn (_,_,a) => a+1) 0 big 60 50)

      val () = section "delete with rebalancing + invariants"
      val () = checkBool "invariants hold (big)" (true, B.checkInvariants ic big)
      (* delete every key one at a time from a scrambled order, checking
         invariants and membership after each removal *)
      val delOrder = perm 200 91   (* gcd(91,200)=1 -> bijection *)
      fun delLoop (t, []) = t
        | delLoop (t, k :: ks) =
            let
              val t' = B.delete ic t k
              val ok = B.checkInvariants ic t'
              val gone = not (B.member ic t' k)
            in
              if ok andalso gone then delLoop (t', ks)
              else raise Fail ("delete broke at key " ^ Int.toString k)
            end
      val emptied =
        (delLoop (big, delOrder); true) handle Fail _ => false
      val () = checkBool "delete-all keeps invariants + removes" (true, emptied)
      val finalT = delLoop (big, delOrder)
      val () = checkBool "tree empty after deleting all" (true, B.isEmpty finalT)
      val () = checkInt "size 0 after deleting all" (0, B.size finalT)
      (* partial deletion preserves remaining keys and order *)
      val half = delLoop (big, List.filter (fn k => k mod 2 = 0) sorted200)
      val () = checkInt "deleted evens -> 100 left" (100, B.size half)
      val () = checkIntList "remaining are odds"
                 (List.filter (fn k => k mod 2 = 1) sorted200, B.keys half)
      val () = checkBool "invariants after partial delete" (true, B.checkInvariants ic half)
      val () = checkBool "delete absent is no-op"
                 (true, B.size (B.delete ic big 999) = 200)
      val () = checkInt "delete then find absent"
                 (0, (case B.find ic (B.delete ic sparse 30) 30 of SOME _ => 1 | NONE => 0))
    in
      Harness.run ()
    end

  val run = runAll
end
