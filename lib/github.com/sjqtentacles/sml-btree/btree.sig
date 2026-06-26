(* btree.sig

   An in-memory B-tree (multiway search tree) of fixed minimum degree t, in pure
   Standard ML.

   Each node holds between t-1 and 2t-1 keys (the root may hold fewer); an
   internal node with n keys has n+1 children, all leaves sit at the same depth.
   This gives O(log n) `insert`, `find`, and `member` with high fan-out.

   Keys are polymorphic: every operation that compares keys takes an explicit
   `cmp : 'k * 'k -> order` (use the same `cmp` for the lifetime of a tree). The
   tree is *persistent*: `insert` returns a new tree and never mutates its
   argument, so old versions remain valid. No FFI, threads, clock or randomness:
   the same inputs always produce the same outputs under MLton and Poly/ML. *)

signature BTREE =
sig
  type ('k, 'v) tree

  val minDegree : int                          (* the order parameter t *)

  val empty    : ('k, 'v) tree
  val isEmpty  : ('k, 'v) tree -> bool
  val size     : ('k, 'v) tree -> int          (* number of bindings *)
  val height   : ('k, 'v) tree -> int          (* 0 for empty, levels otherwise *)

  (* insert k|->v; an existing binding for k is overwritten. *)
  val insert   : ('k * 'k -> order) -> ('k, 'v) tree -> 'k -> 'v -> ('k, 'v) tree
  val find     : ('k * 'k -> order) -> ('k, 'v) tree -> 'k -> 'v option
  val member   : ('k * 'k -> order) -> ('k, 'v) tree -> 'k -> bool

  (* insert k|->v, combining with any existing value: on a collision the new
     binding becomes `combine (old, new)`. `insert` is `insertWith #2`. *)
  val insertWith : ('k * 'k -> order) -> ('v * 'v -> 'v)
                   -> ('k, 'v) tree -> 'k -> 'v -> ('k, 'v) tree
  (* modify the value at k if present (no-op otherwise). *)
  val adjust   : ('k * 'k -> order) -> ('v -> 'v) -> ('k, 'v) tree -> 'k -> ('k, 'v) tree
  (* replace/insert/remove via a function on the optional current value:
     NONE result removes the key, SOME v sets it. *)
  val update   : ('k * 'k -> order) -> ('v option -> 'v option)
                 -> ('k, 'v) tree -> 'k -> ('k, 'v) tree

  (* remove k if present, rebalancing to preserve the B-tree invariants. *)
  val delete   : ('k * 'k -> order) -> ('k, 'v) tree -> 'k -> ('k, 'v) tree

  val fromList : ('k * 'k -> order) -> ('k * 'v) list -> ('k, 'v) tree
  val toList   : ('k, 'v) tree -> ('k * 'v) list   (* ascending by key *)
  val keys     : ('k, 'v) tree -> 'k list          (* ascending *)
  val values   : ('k, 'v) tree -> 'v list          (* by ascending key *)

  (* ordered-map navigation (NONE on empty / no such key) *)
  val min        : ('k, 'v) tree -> ('k * 'v) option   (* smallest binding *)
  val max        : ('k, 'v) tree -> ('k * 'v) option   (* largest binding *)
  val predecessor : ('k * 'k -> order) -> ('k, 'v) tree -> 'k -> ('k * 'v) option (* largest key < k *)
  val successor   : ('k * 'k -> order) -> ('k, 'v) tree -> 'k -> ('k * 'v) option (* smallest key > k *)
  val floor       : ('k * 'k -> order) -> ('k, 'v) tree -> 'k -> ('k * 'v) option (* largest key <= k *)
  val ceiling     : ('k * 'k -> order) -> ('k, 'v) tree -> 'k -> ('k * 'v) option (* smallest key >= k *)

  (* in-order (ascending) traversal *)
  val foldl     : ('k * 'v * 'a -> 'a) -> 'a -> ('k, 'v) tree -> 'a
  val foldr     : ('k * 'v * 'a -> 'a) -> 'a -> ('k, 'v) tree -> 'a
  val app       : ('k * 'v -> unit) -> ('k, 'v) tree -> unit
  val mapValues : ('v -> 'w) -> ('k, 'v) tree -> ('k, 'w) tree
  val filter    : ('k * 'k -> order) -> ('k * 'v -> bool) -> ('k, 'v) tree -> ('k, 'v) tree

  (* bindings with lo <= k <= hi, ascending. *)
  val rangeQuery : ('k * 'k -> order) -> ('k, 'v) tree -> 'k -> 'k -> ('k * 'v) list
  (* ascending fold over bindings with lo <= k <= hi (prunes out-of-range subtrees). *)
  val rangeFold  : ('k * 'k -> order) -> ('k * 'v * 'a -> 'a) -> 'a
                   -> ('k, 'v) tree -> 'k -> 'k -> 'a

  (* assert the structural B-tree invariants; raises Fail with a reason if
     violated. Returns true otherwise (intended for tests). *)
  val checkInvariants : ('k * 'k -> order) -> ('k, 'v) tree -> bool
end
