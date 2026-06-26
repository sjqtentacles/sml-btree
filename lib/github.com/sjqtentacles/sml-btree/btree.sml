(* btree.sml - persistent B-tree of fixed minimum degree t.

   A node is either a leaf holding a sorted list of (key, value) pairs, or an
   internal node holding n sorted keys and n+1 children. Insertion descends to a
   leaf and splits full nodes on the way back up (a functional bottom-up split):
   `ins` returns either the rebuilt node or a (left, median, right) triple that
   the parent absorbs, growing a new root only when the old root splits. *)

structure Btree :> BTREE =
struct

  val minDegree = 3                         (* t: keys per node in [t-1, 2t-1] *)
  val maxKeys = 2 * minDegree - 1           (* = 5 *)

  datatype ('k, 'v) node =
      Leaf of ('k * 'v) list                          (* sorted by key *)
    | Node of ('k * 'v) list * ('k, 'v) node list     (* keys, children=keys+1 *)

  type ('k, 'v) tree = ('k, 'v) node

  val empty = Leaf []

  fun isEmpty (Leaf []) = true
    | isEmpty _ = false

  fun size (Leaf ks) = List.length ks
    | size (Node (ks, cs)) =
        List.length ks + List.foldl (fn (c, a) => a + size c) 0 cs

  fun height (Leaf []) = 0
    | height (Leaf _) = 1
    | height (Node (_, c :: _)) = 1 + height c
    | height (Node (_, [])) = 1   (* malformed; never produced *)

  (* ---- lookup ---- *)
  fun find cmp t k =
    let
      fun inLeaf [] = NONE
        | inLeaf ((k', v) :: rest) =
            (case cmp (k, k') of
                 LESS => NONE
               | EQUAL => SOME v
               | GREATER => inLeaf rest)
      fun go (Leaf ks) = inLeaf ks
        | go (Node (ks, cs)) = descend (ks, cs)
      and descend (k' :: ks', c :: cs') =
            (case cmp (k, #1 k') of
                 LESS => go c
               | EQUAL => SOME (#2 k')
               | GREATER => descend (ks', cs'))
        | descend ([], [c]) = go c
        | descend _ = raise Fail "btree: malformed node"
    in go t end

  fun member cmp t k = Option.isSome (find cmp t k)

  (* ---- insertion ---- *)
  datatype ('k, 'v) ins =
      NoSplit of ('k, 'v) node
    | Split of ('k, 'v) node * ('k * 'v) * ('k, 'v) node

  fun insLeaf cmp (k, v) ks =
    case ks of
        [] => [(k, v)]
      | (k', v') :: rest =>
          (case cmp (k, k') of
               LESS => (k, v) :: ks
             | EQUAL => (k, v) :: rest
             | GREATER => (k', v') :: insLeaf cmp (k, v) rest)

  fun splitLeaf ks =
    let
      val m = List.length ks div 2
      val left = List.take (ks, m)
      val med = List.nth (ks, m)
      val right = List.drop (ks, m + 1)
    in Split (Leaf left, med, Leaf right) end

  fun splitNode (ks, cs) =
    let
      val m = List.length ks div 2
      val leftK = List.take (ks, m)
      val med = List.nth (ks, m)
      val rightK = List.drop (ks, m + 1)
      val leftC = List.take (cs, m + 1)
      val rightC = List.drop (cs, m + 1)
    in Split (Node (leftK, leftC), med, Node (rightK, rightC)) end

  fun insNode cmp (k, v) node =
    case node of
        Leaf ks =>
          let val ks' = insLeaf cmp (k, v) ks
          in if List.length ks' <= maxKeys then NoSplit (Leaf ks')
             else splitLeaf ks'
          end
      | Node (ks, cs) =>
          let
            fun combine (accK, accC, child, ksR, csR) =
              (case insNode cmp (k, v) child of
                   NoSplit child' =>
                     NoSplit (Node (List.rev accK @ ksR,
                                    List.rev accC @ (child' :: csR)))
                 | Split (l, med, r) =>
                     let
                       val ks' = List.rev accK @ (med :: ksR)
                       val cs' = List.rev accC @ (l :: r :: csR)
                     in if List.length ks' <= maxKeys then NoSplit (Node (ks', cs'))
                        else splitNode (ks', cs')
                     end)
            fun walk (accK, accC, [], [lastC]) =
                  combine (accK, accC, lastC, [], [])
              | walk (accK, accC, kk :: ksR, c :: csR) =
                  (case cmp (k, #1 kk) of
                       LESS => combine (accK, accC, c, kk :: ksR, csR)
                     | EQUAL =>
                         NoSplit (Node (List.rev accK @ ((k, v) :: ksR),
                                        List.rev accC @ (c :: csR)))
                     | GREATER => walk (kk :: accK, c :: accC, ksR, csR))
              | walk _ = raise Fail "btree: malformed node"
          in walk ([], [], ks, cs) end

  fun insert cmp t k v =
    case insNode cmp (k, v) t of
        NoSplit n => n
      | Split (l, med, r) => Node ([med], [l, r])

  (* insertWith: like insert but combine (old, new) on an existing key. We add a
     combine-aware leaf/node insert that mirrors insNode. *)
  fun insLeafWith cmp combine (k, v) ks =
    case ks of
        [] => [(k, v)]
      | (k', v') :: rest =>
          (case cmp (k, k') of
               LESS => (k, v) :: ks
             | EQUAL => (k, combine (v', v)) :: rest
             | GREATER => (k', v') :: insLeafWith cmp combine (k, v) rest)

  fun insNodeWith cmp combine (k, v) node =
    case node of
        Leaf ks =>
          let val ks' = insLeafWith cmp combine (k, v) ks
          in if List.length ks' <= maxKeys then NoSplit (Leaf ks')
             else splitLeaf ks'
          end
      | Node (ks, cs) =>
          let
            fun combineC (accK, accC, child, ksR, csR) =
              (case insNodeWith cmp combine (k, v) child of
                   NoSplit child' =>
                     NoSplit (Node (List.rev accK @ ksR,
                                    List.rev accC @ (child' :: csR)))
                 | Split (l, med, r) =>
                     let
                       val ks' = List.rev accK @ (med :: ksR)
                       val cs' = List.rev accC @ (l :: r :: csR)
                     in if List.length ks' <= maxKeys then NoSplit (Node (ks', cs'))
                        else splitNode (ks', cs')
                     end)
            fun walk (accK, accC, [], [lastC]) =
                  combineC (accK, accC, lastC, [], [])
              | walk (accK, accC, kk :: ksR, c :: csR) =
                  (case cmp (k, #1 kk) of
                       LESS => combineC (accK, accC, c, kk :: ksR, csR)
                     | EQUAL =>
                         NoSplit (Node (List.rev accK
                                          @ ((k, combine (#2 kk, v)) :: ksR),
                                        List.rev accC @ (c :: csR)))
                     | GREATER => walk (kk :: accK, c :: accC, ksR, csR))
              | walk _ = raise Fail "btree: malformed node"
          in walk ([], [], ks, cs) end

  fun insertWith cmp combine t k v =
    case insNodeWith cmp combine (k, v) t of
        NoSplit n => n
      | Split (l, med, r) => Node ([med], [l, r])

  (* ---- conversions ---- *)
  fun toList (Leaf ks) = ks
    | toList (Node (ks, cs)) =
        let
          fun go (c :: cs', kv :: ks') = toList c @ (kv :: go (cs', ks'))
            | go ([c], []) = toList c
            | go ([], []) = []
            | go _ = raise Fail "btree: malformed node"
        in go (cs, ks) end

  fun keys t = List.map #1 (toList t)
  fun values t = List.map #2 (toList t)

  fun fromList cmp xs =
    List.foldl (fn ((k, v), t) => insert cmp t k v) empty xs

  (* ---- in-order traversal ---- *)
  fun foldl f acc t =
    let
      fun go (Leaf ks, a) = List.foldl (fn ((k, v), a) => f (k, v, a)) a ks
        | go (Node (ks, cs), a) =
            let
              fun walk (c :: cs', kv :: ks', a) =
                    let val a1 = go (c, a)
                        val a2 = f (#1 kv, #2 kv, a1)
                    in walk (cs', ks', a2) end
                | walk ([c], [], a) = go (c, a)
                | walk ([], [], a) = a
                | walk _ = raise Fail "btree: malformed node"
            in walk (cs, ks, a) end
    in go (t, acc) end

  fun foldr f acc t =
    let
      fun go (Leaf ks, a) = List.foldr (fn ((k, v), a) => f (k, v, a)) a ks
        | go (Node (ks, cs), a) =
            let
              (* fold children/keys from the right: process in reverse *)
              val rcs = List.rev cs
              val rks = List.rev ks
              fun walk (c :: cs', kv :: ks', a) =
                    let val a1 = go (c, a)
                        val a2 = f (#1 kv, #2 kv, a1)
                    in walk (cs', ks', a2) end
                | walk ([c], [], a) = go (c, a)
                | walk ([], [], a) = a
                | walk _ = raise Fail "btree: malformed node"
            in walk (rcs, rks, a) end
    in go (t, acc) end

  fun app f t = foldl (fn (k, v, ()) => f (k, v)) () t

  fun mapValues g (Leaf ks) = Leaf (List.map (fn (k, v) => (k, g v)) ks)
    | mapValues g (Node (ks, cs)) =
        Node (List.map (fn (k, v) => (k, g v)) ks, List.map (mapValues g) cs)

  fun filter cmp p t =
    fromList cmp (List.filter p (toList t))

  (* ---- ordered-map navigation ---- *)
  fun min (Leaf []) = NONE
    | min (Leaf (kv :: _)) = SOME kv
    | min (Node (_, c :: _)) = min c
    | min (Node (_, [])) = NONE

  fun max (Leaf []) = NONE
    | max (Leaf ks) = SOME (List.last ks)
    | max (Node (_, cs)) = (case cs of [] => NONE | _ => max (List.last cs))

  (* Generic boundary search over the in-order list. These are simple and
     correct; the tree-descending variants would be faster but the list-based
     version keeps the semantics unambiguous. *)
  fun floor cmp t k =
    foldl (fn (k', v', acc) =>
             if cmp (k', k) <> GREATER then SOME (k', v') else acc) NONE t

  fun ceiling cmp t k =
    foldr (fn (k', v', acc) =>
             if cmp (k', k) <> LESS then SOME (k', v') else acc) NONE t

  fun predecessor cmp t k =
    foldl (fn (k', v', acc) =>
             if cmp (k', k) = LESS then SOME (k', v') else acc) NONE t

  fun successor cmp t k =
    foldr (fn (k', v', acc) =>
             if cmp (k', k) = GREATER then SOME (k', v') else acc) NONE t

  (* ---- deletion with borrow/merge rebalancing ----
     Standard B-tree delete. We operate on a node and remove key k, fixing up
     any child that underflows (< t-1 keys) by borrowing from a sibling or
     merging. `delete` then shrinks the root if it becomes an empty internal
     node (single child) or an empty leaf. *)

  fun nodeKeyCount (Leaf ks) = List.length ks
    | nodeKeyCount (Node (ks, _)) = List.length ks

  local
    val minKeys = minDegree - 1

    (* replace the i-th element of a list *)
    fun listSet (xs, i, x) =
      List.tabulate (List.length xs, fn j => if j = i then x else List.nth (xs, j))

    (* fix child i of (ks, cs) if it underflows; returns rebalanced (ks, cs). *)
    fun fixChild (ks, cs, i) =
      let
        val child = List.nth (cs, i)
      in
        if nodeKeyCount child >= minKeys then (ks, cs)
        else
          let
            val hasLeft = i > 0
            val hasRight = i < List.length cs - 1
            val leftSib = if hasLeft then SOME (List.nth (cs, i - 1)) else NONE
            val rightSib = if hasRight then SOME (List.nth (cs, i + 1)) else NONE
          in
            case (leftSib, rightSib) of
                (SOME ls, _) =>
                  if nodeKeyCount ls > minKeys then
                    (* borrow from left sibling *)
                    borrowLeft (ks, cs, i)
                  else (case rightSib of
                            SOME rs =>
                              if nodeKeyCount rs > minKeys then borrowRight (ks, cs, i)
                              else mergeChildren (ks, cs, i - 1)  (* merge i-1, i *)
                          | NONE => mergeChildren (ks, cs, i - 1))
              | (NONE, SOME rs) =>
                  if nodeKeyCount rs > minKeys then borrowRight (ks, cs, i)
                  else mergeChildren (ks, cs, i)  (* merge i, i+1 *)
              | (NONE, NONE) => (ks, cs)  (* root single child; handled by caller *)
          end
      end

    (* borrow one key from left sibling into child i, rotating through separator
       ks[i-1]. *)
    and borrowLeft (ks, cs, i) =
      let
        val sep = List.nth (ks, i - 1)
        val left = List.nth (cs, i - 1)
        val child = List.nth (cs, i)
      in
        case (left, child) of
            (Leaf lks, Leaf cks) =>
              let
                val borrowed = List.last lks
                val left' = Leaf (List.take (lks, List.length lks - 1))
                val child' = Leaf (sep :: cks)
                val ks' = listSet (ks, i - 1, borrowed)
                val cs' = listSet (listSet (cs, i - 1, left'), i, child')
              in (ks', cs') end
          | (Node (lks, lcs), Node (cks, ccs)) =>
              let
                val borrowedK = List.last lks
                val borrowedC = List.last lcs
                val left' = Node (List.take (lks, List.length lks - 1),
                                  List.take (lcs, List.length lcs - 1))
                val child' = Node (sep :: cks, borrowedC :: ccs)
                val ks' = listSet (ks, i - 1, borrowedK)
                val cs' = listSet (listSet (cs, i - 1, left'), i, child')
              in (ks', cs') end
          | _ => raise Fail "btree: borrowLeft mixed node kinds"
      end

    (* borrow one key from right sibling into child i, rotating through ks[i]. *)
    and borrowRight (ks, cs, i) =
      let
        val sep = List.nth (ks, i)
        val child = List.nth (cs, i)
        val right = List.nth (cs, i + 1)
      in
        case (child, right) of
            (Leaf cks, Leaf rks) =>
              let
                val borrowed = List.hd rks
                val right' = Leaf (List.tl rks)
                val child' = Leaf (cks @ [sep])
                val ks' = listSet (ks, i, borrowed)
                val cs' = listSet (listSet (cs, i, child'), i + 1, right')
              in (ks', cs') end
          | (Node (cks, ccs), Node (rks, rcs)) =>
              let
                val borrowedK = List.hd rks
                val borrowedC = List.hd rcs
                val right' = Node (List.tl rks, List.tl rcs)
                val child' = Node (cks @ [sep], ccs @ [borrowedC])
                val ks' = listSet (ks, i, borrowedK)
                val cs' = listSet (listSet (cs, i, child'), i + 1, right')
              in (ks', cs') end
          | _ => raise Fail "btree: borrowRight mixed node kinds"
      end

    (* merge child i with child i+1, pulling down separator ks[i]. *)
    and mergeChildren (ks, cs, i) =
      let
        val sep = List.nth (ks, i)
        val left = List.nth (cs, i)
        val right = List.nth (cs, i + 1)
        val merged =
          case (left, right) of
              (Leaf lks, Leaf rks) => Leaf (lks @ (sep :: rks))
            | (Node (lks, lcs), Node (rks, rcs)) =>
                Node (lks @ (sep :: rks), lcs @ rcs)
            | _ => raise Fail "btree: mergeChildren mixed node kinds"
        fun removeAt (xs, j) =
          List.take (xs, j) @ List.drop (xs, j + 1)
        val ks' = removeAt (ks, i)
        val cs' = listSet (removeAt (cs, i + 1), i, merged)
      in (ks', cs') end

    (* delete the maximum binding of a subtree, returning (kv, subtree'). *)
    fun delMax (Leaf ks) =
          let val kv = List.last ks
          in (kv, Leaf (List.take (ks, List.length ks - 1))) end
      | delMax (Node (ks, cs)) =
          let
            val i = List.length cs - 1
            val (kv, c') = delMax (List.nth (cs, i))
            val cs1 = listSet (cs, i, c')
            val (ks', cs') = fixChild (ks, cs1, i)
          in (kv, Node (ks', cs')) end

    fun del cmp k node =
      case node of
          Leaf ks => Leaf (List.filter (fn (k', _) => cmp (k', k) <> EQUAL) ks)
        | Node (ks, cs) =>
            let
              (* find position: index i of first key >= k *)
              fun locate ([], _) = (List.length ks, NONE)  (* descend last child *)
                | locate ((kk :: rest), idx) =
                    (case cmp (k, #1 kk) of
                         LESS => (idx, NONE)
                       | EQUAL => (idx, SOME kk)
                       | GREATER => locate (rest, idx + 1))
              val (i, found) = locate (ks, 0)
            in
              case found of
                  SOME _ =>
                    (* key is in this internal node: replace with predecessor
                       (max of left child), then delete that from left child. *)
                    let
                      val leftC = List.nth (cs, i)
                      val (predKV, leftC') = delMax leftC
                      val ks1 = listSet (ks, i, predKV)
                      val cs1 = listSet (cs, i, leftC')
                      val (ks', cs') = fixChild (ks1, cs1, i)
                    in Node (ks', cs') end
                | NONE =>
                    (* descend into child i *)
                    let
                      val c' = del cmp k (List.nth (cs, i))
                      val cs1 = listSet (cs, i, c')
                      val (ks', cs') = fixChild (ks, cs1, i)
                    in Node (ks', cs') end
            end
  in
    fun delete cmp t k =
      if not (member cmp t k) then t
      else
        case del cmp k t of
            Node (ks, cs) =>
              (* shrink root: if internal root has no keys, its single child
                 becomes the new root *)
              (case ks of
                   [] => (case cs of [only] => only | _ => Node (ks, cs))
                 | _ => Node (ks, cs))
          | leaf => leaf
  end

  fun adjust cmp g t k =
    case find cmp t k of
        NONE => t
      | SOME v => insert cmp t k (g v)

  fun update cmp g t k =
    case g (find cmp t k) of
        NONE => delete cmp t k
      | SOME v => insert cmp t k v

  fun rangeQuery cmp t lo hi =
    List.filter
      (fn (k, _) => cmp (lo, k) <> GREATER andalso cmp (k, hi) <> GREATER)
      (toList t)

  (* ascending fold over [lo, hi], descending the tree and pruning subtrees that
     lie entirely outside the range. *)
  fun rangeFold cmp f acc t lo hi =
    let
      fun inRange k = cmp (lo, k) <> GREATER andalso cmp (k, hi) <> GREATER
      fun go (Leaf ks, a) =
            List.foldl (fn ((k, v), a) => if inRange k then f (k, v, a) else a) a ks
        | go (Node (ks, cs), a) =
            let
              fun walk (c :: cs', kv :: ks', a) =
                    let
                      (* descend into c only if some key there could be >= lo,
                         i.e. unless this whole subtree is < lo. The separator kv
                         is greater than all keys in c, so if kv < lo skip c. *)
                      val (k', v') = kv
                      val a1 = if cmp (#1 kv, lo) = LESS then a else go (c, a)
                      val a2 = if inRange k' then f (k', v', a1) else a1
                      (* if separator already exceeds hi, no later key qualifies *)
                    in
                      if cmp (k', hi) = GREATER then a1
                      else walk (cs', ks', a2)
                    end
                | walk ([c], [], a) = go (c, a)
                | walk ([], [], a) = a
                | walk _ = raise Fail "btree: malformed node"
            in walk (cs, ks, a) end
    in go (t, acc) end

  (* ---- invariant checker (for tests) ---- *)
  fun checkInvariants cmp t =
    let
      val minKeys = minDegree - 1
      fun fail s = raise Fail ("btree invariant: " ^ s)
      fun sortedStrict ks =
        let
          fun go [] = true
            | go [_] = true
            | go ((k1,_) :: (rest as (k2,_) :: _)) =
                cmp (k1, k2) = LESS andalso go rest
        in go ks end
      (* returns the uniform leaf depth of the subtree *)
      fun chk (Leaf ks, isRoot) =
            (if not (sortedStrict ks) then fail "leaf keys unsorted" else ();
             if not isRoot andalso List.length ks < minKeys then fail "leaf underflow" else ();
             if List.length ks > maxKeys then fail "leaf overflow" else ();
             0)
        | chk (Node (ks, cs), isRoot) =
            let
              val nk = List.length ks
            in
              if not (sortedStrict ks) then fail "node keys unsorted" else ();
              if List.length cs <> nk + 1 then fail "child count <> keys+1" else ();
              if nk > maxKeys then fail "node overflow" else ();
              if not isRoot andalso nk < minKeys then fail "node underflow" else ();
              if isRoot andalso nk < 1 then fail "root must have >= 1 key" else ();
              let
                val depths = List.map (fn c => chk (c, false)) cs
              in
                case depths of
                    [] => fail "internal node with no children"
                  | d :: ds =>
                      if List.all (fn d' => d' = d) ds then 1 + d
                      else fail "leaves at differing depths"
              end
            end
    in
      (case t of
           Leaf _ => ignore (chk (t, true))
         | Node _ => ignore (chk (t, true)));
      true
    end
end
