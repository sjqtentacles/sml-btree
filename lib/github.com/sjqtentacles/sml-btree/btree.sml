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

  fun rangeQuery cmp t lo hi =
    List.filter
      (fn (k, _) => cmp (lo, k) <> GREATER andalso cmp (k, hi) <> GREATER)
      (toList t)
end
