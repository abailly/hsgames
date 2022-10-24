||| An implementation of `Data.Heap` based on left-skewed binary trees.
|||
||| This implementation is an adaptation of Okasaki's `LeftistHeap`
||| found on pp.18-20 of his book, and of this
||| [Agda](http://ics.p.lodz.pl/~stolarek/_media/pl:research:dep-typed-wbl-heaps.pdf)
||| implementation that looked reasonably easy to port.
|||
||| Note that contrary to the paper, we don't index our `LeftistBinTree` with a
||| `Priority` because this would require to _tighten_ the constraint to bounded
||| types: We must have a _lowest_ bound when we construct a singleton tree.
module Data.Heap.LeftistHeap

import public Data.Heap

import Data.Nat
import public Data.Nat.Order

%default total

namespace RawTree

  ||| A implementation of a leftist binary tree which is not `Nat`-indexed
  data LeftistBinTree : (a : Type) -> Type where
    Empty : LeftistBinTree a
    Node : (elem : a) -> (rank : Int) -> (left : LeftistBinTree a) -> (right : LeftistBinTree a)
         -> LeftistBinTree a

  rank : LeftistBinTree a -> Int
  rank Empty = 0
  rank (Node _ r _ _) = r

  makeNode : (elem : a) -> LeftistBinTree a -> LeftistBinTree a -> LeftistBinTree a
  makeNode elem left right =
    if rank left < rank right
    then Node elem (rank left + rank right) right left
    else Node elem (rank left + rank right) left right

  mergeTree : (Ord a) => (left : LeftistBinTree a) -> (right : LeftistBinTree a) -> LeftistBinTree a
  mergeTree Empty right = right
  mergeTree left  Empty = left
  mergeTree l@(Node elem rank left right) r@(Node elem' rank' left' right') =
    if (elem < elem')
    then  makeNode elem left (mergeTree right r)
    else  makeNode elem' left' (mergeTree l right')

  findMin : LeftistBinTree a -> Maybe a
  findMin Empty           = Nothing
  findMin (Node elem _ _ _) = Just elem

  popMin : (Ord a) => LeftistBinTree a -> (LeftistBinTree a, Maybe a)
  popMin Empty  = (Empty, Nothing)
  popMin (Node elem _ left right) = (mergeTree left right, Just elem)

  insert : (Ord a) => a -> LeftistBinTree a -> LeftistBinTree a
  insert x Empty = Node x 1 Empty Empty
  insert x node  = mergeTree (Node x 1 Empty Empty) node



||| A `LeftistBinTree` is a binary tree decorated with `rank` informations.
|||
||| This is a `Nat`-indexed data structure that forms the basis for a proper
||| heap respecting the `Heap` interface.
||| @rank the rank of the tree, eg. the number of elements it contains
||| @a The type of elements stored in the tree
data LeftistBinTree : (rank : Nat) -> (a : Type) -> Type where

  ||| The empty tree
  Empty : LeftistBinTree Z a

  ||| An element in the `Tree`
  |||
  ||| A `Node` in the tree is built in such a way that it respects the _leftist_
  ||| property: The rank of the `left` node is greater than or equal to the rank
  ||| of the `right` node. Furthermore it must respect the _priority_ property
  ||| which implies that `elem` is lower than equal to elements in `left`  and
  ||| `right` sub-trees.
  |||
  ||| @elem the element contained in the node
  ||| @left the left branch of the tree with rank `k`
  ||| @right the right branch of the tree with rank `n`
  ||| @prfLeftist a proof that the rank of `left` is greater than or equal to the
  ||| the rank of `right`
  Node : {k, n : Nat}
       -> (elem : a)
       -> (left : LeftistBinTree k a) -> (right : LeftistBinTree n a)
       -> { auto prfLeftist : LTE n k }
       -> LeftistBinTree (S (k + n)) a

makeNode : {r, q : Nat} -> (elem : a) -> LeftistBinTree r a -> LeftistBinTree q a -> LeftistBinTree (S (r + q)) a
makeNode elem x y with (Order.lte r q)
  makeNode elem x y | (Yes z) = rewrite plusCommutative r q in Node elem y x
  makeNode elem x y | (No z) = let prf = lteSuccLeft $ notLTEImpliesGT z in Node elem x y

||| Merge 2 `LeftistBinTree`s while preserving properties.
|||
||| @left first tree to merge
||| @right second tree to merge
mergeTree : (Ord a) => {r, q: Nat} -> (left : LeftistBinTree r a) -> (right : LeftistBinTree q a) -> LeftistBinTree (r + q) a
mergeTree Empty right = right
mergeTree left  Empty {r} = rewrite plusZeroRightNeutral r in left
mergeTree (Node elem left right {k} {n}) (Node elem' left' right' {k=k1} {n=n1}) with (elem < elem')
  mergeTree (Node elem left right {k} {n}) (Node elem' left' right' {k=k1} {n=n1}) | True =
    rewrite sym (plusAssociative k n (S (k1 + n1))) in
    makeNode elem left (mergeTree right (Node elem' left' right'))
  mergeTree (Node elem left right {k} {n}) (Node elem' left' right' {k=k1} {n=n1}) | False =
    rewrite sym (plusSuccRightSucc (k + n) (k1 + n1)) in
    rewrite plusCommutative (plus k n) (plus k1 n1) in
    rewrite plusSuccRightSucc (k1 + n1) (k + n) in
    rewrite sym (plusAssociative k1 n1 (S (k + n))) in
    rewrite plusCommutative n1 (S (plus k n)) in
    -- this assert_total sucks but I can't find an easy way to convince the TC the recursive call
    -- is smaller, which it obviously is (?)
    makeNode elem' left' (assert_total $ mergeTree (Node elem left right) right')

findMin : LeftistBinTree r a -> Maybe a
findMin Empty           = Nothing
findMin (Node elem _ _) = Just elem

popMin : (Ord a) => LeftistBinTree (S r) a -> (LeftistBinTree r a, a)
popMin Empty impossible
popMin (Node elem left right) = (mergeTree left right, elem)

insert : (Ord a) => a -> {r : Nat} -> LeftistBinTree r a -> LeftistBinTree (S r) a
insert x Empty = Node x Empty Empty
insert x node  = mergeTree (Node x Empty Empty) node

||| a `BinaryHeap` is a heap backed by a `LeftistBinTree`
public export
data BinaryHeap : Type -> Type where

  ||| Wrapper around a dependent product of a `rank` and a tree of the
  ||| given rank.
  |||
  ||| This allows us to "hide" the indexed nature of the binary tree and to
  ||| provide a proper implementation of `Heap` interface.
  |||
  BinHeap : ( n : Nat ** LeftistBinTree n a) -> BinaryHeap a

EmptyHeap : BinaryHeap a
EmptyHeap = BinHeap (Z ** Empty)

-- implementation of Heap
public export
Heap BinaryHeap where

  empty = EmptyHeap

  isEmpty (BinHeap (Z ** _)) = True
  isEmpty (BinHeap _) = False

  push elem (BinHeap (n ** tree)) = BinHeap (S n ** insert elem tree)

  peek (BinHeap (n ** tree)) = findMin tree

  pop (BinHeap (Z ** Empty))  = (EmptyHeap, Nothing)
  pop (BinHeap (S n ** tree)) = let (tree', x) = popMin tree
                                in (BinHeap (n ** tree'), Just x)

  merge (BinHeap (n ** left)) (BinHeap (m ** right)) =
    BinHeap (n + m ** mergeTree left right)

  stats = const Nothing
