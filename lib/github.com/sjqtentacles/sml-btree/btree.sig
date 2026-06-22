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

  val fromList : ('k * 'k -> order) -> ('k * 'v) list -> ('k, 'v) tree
  val toList   : ('k, 'v) tree -> ('k * 'v) list   (* ascending by key *)
  val keys     : ('k, 'v) tree -> 'k list          (* ascending *)
  val values   : ('k, 'v) tree -> 'v list          (* by ascending key *)

  (* bindings with lo <= k <= hi, ascending. *)
  val rangeQuery : ('k * 'k -> order) -> ('k, 'v) tree -> 'k -> 'k -> ('k * 'v) list
end
