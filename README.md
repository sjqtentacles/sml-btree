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
  val insert   : ('k * 'k -> order) -> ('k, 'v) tree -> 'k -> 'v -> ('k, 'v) tree
  val find     : ('k * 'k -> order) -> ('k, 'v) tree -> 'k -> 'v option
  val member   : ('k * 'k -> order) -> ('k, 'v) tree -> 'k -> bool
  val fromList : ('k * 'k -> order) -> ('k * 'v) list -> ('k, 'v) tree
  val toList   : ('k, 'v) tree -> ('k * 'v) list   (* ascending by key *)
  val keys     : ('k, 'v) tree -> 'k list
  val values   : ('k, 'v) tree -> 'v list
  val rangeQuery : ('k * 'k -> order) -> ('k, 'v) tree -> 'k -> 'k -> ('k * 'v) list
end
```

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
  btree.sml      B-tree implementation (leaf/internal nodes, split-on-insert)
  sources.mlb    ordered source list
  btree.mlb      public basis
examples/
  demo.sml       B-tree walkthrough
test/
  harness.sml    shared assertion harness
  test.sml       split + scrambled 1..200 vectors (44 checks)
  entry.sml / main.sml
tools/polybuild  Poly/ML build wrapper
```

## Tests

44 deterministic checks. B-tree invariants are verified indirectly: a tree built
from a scrambled permutation of `1..200` (`i |-> i*73 mod 200`) must enumerate
in ascending order, make every key retrievable with the right value, track
`size`, and keep `height` logarithmic (so node splits are exercised). Also
covers the explicit root split when the 6th key is added to a degree-3 leaf,
insertion-order independence (ascending vs descending builds), duplicate-key
overwrite, `rangeQuery`, and a string-keyed tree. Run `make all-tests` to verify
identical output under both compilers.

## License

MIT. See [LICENSE](LICENSE).
