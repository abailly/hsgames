||| An implementation of `Data.Heap` based on left-skewed binary trees.
||| This implementation is an adaptation of Okasaki's `LeftistHeap`
||| found on pp.18-20 of his book.
|||
||| * an [Agda](http://ics.p.lodz.pl/~stolarek/_media/pl:research:dep-typed-wbl-heaps.pdf) implementation that looks reasonably easy to port
module Data.Heap.LeftistHeap

import Data.Heap
import Decidable.Order

||| A `LeftistHeap` is a binary tree decorated with `rank` informations
||| @rank the rank of the heap, eg. the number of elements it contains
||| @a The type of elements stored in the heap
data LeftistHeap : (rank : Nat) -> (a : Type) -> Type where

  ||| The empty heap
  Empty : LeftistHeap Z a

  ||| An element in the `Heap`
  |||
  ||| A `Node` in the heap is built in such a way that it respects the _leftist_
  ||| property: The rank of the `left` node is greater than or equal to the rank
  ||| of the `right` node. Furthermore it must respect the _priority_ property
  ||| which implies that `elem` is lower than equal to elements in `left`  and
  ||| `right` sub-heaps.
  |||
  ||| @elem the element contained in the node
  ||| @left the left branch of the tree with rank `k`
  ||| @right the right branch of the tree with rank `n`
  ||| @prfLeftist a proof that the rank of `left` is greater than or equal to the
  ||| the rank of `right`
  Node : (elem : a)
       -> (left : LeftistHeap k a) -> (right : LeftistHeap n a)
       -> { auto prfLeftist : LTE n k }
       -> LeftistHeap (S (k + n)) a

makeNode : (elem : a) -> LeftistHeap r a -> LeftistHeap q a -> LeftistHeap (S (r + q)) a
makeNode elem x y {r} {q} with (order {to=LTE} r q)
  makeNode elem x y {r = r} {q = q} | (Left l)  = rewrite plusCommutative r q in Node elem y x
  makeNode elem x y {r = r} {q = q} | (Right z) = Node elem x y

||| Merge 2 `LeftistHeap`s while preserving properties.
|||
||| @left first heap to merge
||| @right second heap to merge
mergeHeap : (Ord a) => (left : LeftistHeap r a) -> (right : LeftistHeap q a) -> LeftistHeap (r + q) a
mergeHeap Empty right = right
mergeHeap left  Empty {r} = rewrite plusZeroRightNeutral r in left
mergeHeap (Node elem left right {k} {n}) (Node elem' left' right' {k=k1} {n=n1}) with (elem < elem')
  mergeHeap (Node elem left right {k} {n}) (Node elem' left' right' {k=k1} {n=n1}) | True =
    rewrite sym (plusAssociative k n (S (k1 + n1))) in
    makeNode elem left (mergeHeap right (Node elem' left' right'))
  mergeHeap (Node elem left right {k} {n}) (Node elem' left' right' {k=k1} {n=n1}) | False =
    rewrite sym (plusSuccRightSucc (k + n) (k1 + n1)) in
    rewrite plusCommutative (plus k n) (plus k1 n1) in
    rewrite plusSuccRightSucc (k1 + n1) (k + n) in
    rewrite sym (plusAssociative k1 n1 (S (k + n))) in
    rewrite plusCommutative n1 (S (plus k n)) in
    makeNode elem' left' (mergeHeap (Node elem left right) right')

findMin : LeftistHeap r a -> Maybe a
findMin Empty           = Nothing
findMin (Node elem _ _) = Just elem

popMin : (Ord a) => LeftistHeap (S r) a -> (LeftistHeap r a, Maybe a)
popMin Empty impossible
popMin (Node elem left right) = (mergeHeap left right, Just elem)

insert : (Ord a) => a -> LeftistHeap r a -> LeftistHeap (S r) a
insert x Empty = Node x Empty Empty
insert x node  = mergeHeap (Node x Empty Empty) node
