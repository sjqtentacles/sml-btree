# sml-btree

[![CI](https://github.com/sjqtentacles/sml-btree/actions/workflows/ci.yml/badge.svg)](https://github.com/sjqtentacles/sml-btree/actions/workflows/ci.yml)

An in-memory **B-tree** (multiway search tree) of fixed minimum degree `t = 3`,
in pure Standard ML — high-fan-out ordered maps with O(log n) `insert`, `find`,
and `member`.

Each node holds between `t-1` and `2t-1` keys (the root may hold fewer); an
internal node with `n` keys has `n+1` children and all leaves sit at the same
depth. Insertion descends to a leaf and splits full nodes on the way back up.

Keys are **polymorphic**: every operation that compares keys takes an explicit
`cmp : 'k * 'k -> order`, so one library serves int keys, string keys, or any
ordered type without functors. The tree is **persistent**: `insert` returns a
new tree and never mutates its argument, so old versions stay valid.

No dependencies, no FFI, no threads, no clock, no randomness: the same inputs
always produce the same outputs under **MLton** and **Poly/ML**.

## API

```sml
structure Btree : sig
  type ('k, 'v) tree
  val minDegree : int
  val empty    : ('k, 'v) tree
  val isEmpty  : ('k, 'v) tree -> bool
  val size     : ('k, 'v) tree -> int
  val height   : ('k, 'v) tree -> int

  (* lookups *)
  val find     : ('k * 'k -> order) -> ('k, 'v) tree -> 'k -> 'v option
  val member   : ('k * 'k -> order) -> ('k, 'v) tree -> 'k -> bool

  (* updates (all persistent) *)
  val insert     : ('k * 'k -> order) -> ('k, 'v) tree -> 'k -> 'v -> ('k, 'v) tree
  val insertWith : ('k * 'k -> order) -> ('v * 'v -> 'v)
                   -> ('k, 'v) tree -> 'k -> 'v -> ('k, 'v) tree
  val adjust   : ('k * 'k -> order) -> ('v -> 'v) -> ('k, 'v) tree -> 'k -> ('k, 'v) tree
  val update   : ('k * 'k -> order) -> ('v option -> 'v option)
                 -> ('k, 'v) tree -> 'k -> ('k, 'v) tree
  val delete   : ('k * 'k -> order) -> ('k, 'v) tree -> 'k -> ('k, 'v) tree

  (* conversions *)
  val fromList : ('k * 'k -> order) -> ('k * 'v) list -> ('k, 'v) tree
  val toList   : ('k, 'v) tree -> ('k * 'v) list   (* ascending by key *)
  val keys     : ('k, 'v) tree -> 'k list
  val values   : ('k, 'v) tree -> 'v list

  (* ordered-map navigation (NONE on empty / no such key) *)
  val min         : ('k, 'v) tree -> ('k * 'v) option
  val max         : ('k, 'v) tree -> ('k * 'v) option
  val predecessor : ('k * 'k -> order) -> ('k, 'v) tree -> 'k -> ('k * 'v) option
  val successor   : ('k * 'k -> order) -> ('k, 'v) tree -> 'k -> ('k * 'v) option
  val floor       : ('k * 'k -> order) -> ('k, 'v) tree -> 'k -> ('k * 'v) option
  val ceiling     : ('k * 'k -> order) -> ('k, 'v) tree -> 'k -> ('k * 'v) option

  (* in-order (ascending) traversal *)
  val foldl     : ('k * 'v * 'a -> 'a) -> 'a -> ('k, 'v) tree -> 'a
  val foldr     : ('k * 'v * 'a -> 'a) -> 'a -> ('k, 'v) tree -> 'a
  val app       : ('k * 'v -> unit) -> ('k, 'v) tree -> unit
  val mapValues : ('v -> 'w) -> ('k, 'v) tree -> ('k, 'w) tree
  val filter    : ('k * 'k -> order) -> ('k * 'v -> bool) -> ('k, 'v) tree -> ('k, 'v) tree

  (* range queries over lo <= k <= hi *)
  val rangeQuery : ('k * 'k -> order) -> ('k, 'v) tree -> 'k -> 'k -> ('k * 'v) list
  val rangeFold  : ('k * 'k -> order) -> ('k * 'v * 'a -> 'a) -> 'a
                   -> ('k, 'v) tree -> 'k -> 'k -> 'a

  (* assert the structural B-tree invariants (for tests) *)
  val checkInvariants : ('k * 'k -> order) -> ('k, 'v) tree -> bool
end
```

`insert` overwrites an existing binding; `insertWith combine` instead stores
`combine (old, new)` on a collision (so `insert = insertWith #2`). `update`
takes the optional current value and returns `NONE` to delete or `SOME v` to
set, covering insert/modify/remove in one call. `delete` removes a key with
full borrow-from-sibling / merge-children rebalancing, shrinking the root when
an internal node empties, so the `[t-1, 2t-1]` key bounds and uniform leaf
depth are preserved (asserted by `checkInvariants` in the test suite).

`floor`/`ceiling` return the nearest binding `<= k` / `>= k`;
`predecessor`/`successor` are their strict (`< k` / `> k`) variants.

The same `cmp` must be used for the lifetime of a tree (mixing comparisons gives
unspecified results, exactly as for a hand-rolled search tree).

## Example

```sml
val ic = Int.compare
val t  = Btree.fromList ic [(5,50),(3,30),(7,70),(1,10),(9,90),(2,20)]
val [1,2,3,5,7,9] = Btree.keys t           (* ascending *)
val SOME 70       = Btree.find ic t 7
val t'            = Btree.insert ic t 4 40  (* persistent: t is unchanged *)
val [(2,20),(3,30),(4,40)] = Btree.rangeQuery ic t' 2 4
val SOME (1,10)   = Btree.min t'
val SOME (3,30)   = Btree.floor ic t' 4       (* largest key <= 4 *)
val t''           = Btree.delete ic t' 3      (* rebalances, 3 removed *)
val false         = Btree.member ic t'' 3
val 27            = Btree.foldl (fn (k,_,a) => a+k) 0 t  (* 1+2+3+5+7+9 *)
```

Running [`examples/demo.sml`](examples/demo.sml) with `make example` prints:

```
B-tree minimum degree t = 3 (a node holds 2..5 keys)

Inserting keys 1..5 (fits one leaf), then 6 (forces a split):
  after 1..5: height = 1, size = 5
  after +6:   height = 2, size = 6
  keys        = [1,2,3,4,5,6]

Bulk-loading a scrambled 1..100 (value = key*key):
  size        = 100
  height      = 3
  find 42     = 1764
  member 100  = true
  member 101  = false
  range 10..15 keys = [10,11,12,13,14,15]
  first 8 keys      = [1,2,3,4,5,6,7,8]
```

## Build & test

Requires [MLton](http://mlton.org/) and/or [Poly/ML](https://polyml.org/).

```sh
make test        # build + run the suite under MLton
make test-poly   # run the suite under Poly/ML
make all-tests   # both
make example     # build + run the demo
make clean
```

## Installing with smlpkg

```sh
smlpkg add github.com/sjqtentacles/sml-btree
smlpkg sync
```

Reference `lib/github.com/sjqtentacles/sml-btree/btree.mlb` from your own
`.mlb` (MLton / MLKit), or feed `sources.mlb` to `tools/polybuild` (Poly/ML).

## Layout

```
sml.pkg                                       smlpkg manifest
Makefile                                      MLton + Poly/ML targets
.github/workflows/ci.yml                      CI: MLton + Poly/ML
lib/github.com/sjqtentacles/sml-btree/
  btree.sig      BTREE signature
  btree.sml      B-tree (leaf/internal nodes, split-on-insert, borrow/merge delete)
  sources.mlb    ordered source list
  btree.mlb      public basis
examples/
  demo.sml       B-tree walkthrough
test/
  harness.sml    shared assertion harness
  test.sml       split + scrambled 1..200 vectors, delete stress (88 checks)
  entry.sml / main.sml
tools/polybuild  Poly/ML build wrapper
```

## Tests

88 deterministic checks. B-tree invariants are verified directly via
`checkInvariants` (key bounds `[t-1, 2t-1]`, `n+1` children per `n` keys, and
uniform leaf depth) and indirectly: a tree built from a scrambled permutation
of `1..200` (`i |-> i*73 mod 200`) must enumerate in ascending order, make
every key retrievable with the right value, track `size`, and keep `height`
logarithmic. Deletion is stress-tested by removing all 200 keys in a scrambled
order (and a half-deletion), asserting the invariants and membership after
every step. Also covers the explicit root split, insertion-order independence,
duplicate-key overwrite, `insertWith`/`adjust`/`update`, the folds, ordered-map
navigation (`min`/`max`/`floor`/`ceiling`/`predecessor`/`successor`),
`rangeQuery`/`rangeFold`, and a string-keyed tree. Run `make all-tests` to
verify identical output under both compilers.

## License

MIT. See [LICENSE](LICENSE).
