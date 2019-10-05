||| An implementation of `Data.Heap` based on left-skewed binary trees.
|||
||| This implementation is a direct adaptation of Okasaki's `LeftistHeap`
||| found on pp.18-20 of his book without trying to lift anything at the
||| Type-level nor prove any properties. Its purpose is to be used as a
||| baseline for performance analysis and improvements in the "proven"
||| version found in `Data.Heap.LeftistHeap`.
module Data.Heap.RawBinaryHeap

import public Data.Heap

||| A implementation of a leftist binary tree which is not `Nat`-indexed
public export
data BinaryTree : (a : Type) -> Type where
  Empty : BinaryTree a
  Node : (elem : a) -> (rank : Int) -> (left : BinaryTree a) -> (right : BinaryTree a)
       -> BinaryTree a

rank : BinaryTree a -> Int
rank Empty = 0
rank (Node _ rk _ _) = rk

makeNode : (elem : a) -> BinaryTree a -> BinaryTree a -> BinaryTree a
makeNode elem left right =
  if rank left < rank right
  then Node elem (rank left + rank right) right left
  else Node elem (rank left + rank right) left right

mergeTree : (Ord a) => (left : BinaryTree a) -> (right : BinaryTree a) -> BinaryTree a
mergeTree Empty right = right
mergeTree left  Empty = left
mergeTree l@(Node elem rank left right) r@(Node elem' rank' left' right') =
  if (elem < elem')
  then  makeNode elem left (mergeTree right r)
  else  makeNode elem' left' (mergeTree l right')

findMin : BinaryTree a -> Maybe a
findMin Empty           = Nothing
findMin (Node elem _ _ _) = Just elem

popMin : (Ord a) => BinaryTree a -> (BinaryTree a, Maybe a)
popMin Empty  = (Empty, Nothing)
popMin (Node elem _ left right) = (mergeTree left right, Just elem)

insert : (Ord a) => a -> BinaryTree a -> BinaryTree a
insert x Empty = Node x 1 Empty Empty
insert x node  = mergeTree (Node x 1 Empty Empty) node

public export
data BinaryHeap : Type -> Type where

  ||| A Heap that keeps track of elements that's been inserted into
  ||| it.
  ||| For debugging and analysis purpose
  MkHeap : (tree : BinaryTree a) -> (inserts: Lazy (List a)) -> BinaryHeap a

traceInserts : BinaryHeap a -> List a
traceInserts (MkHeap tree inserts) = inserts

-- implementation of Heap
public export
Heap BinaryHeap where

  empty = MkHeap Empty []

  isEmpty (MkHeap Empty _) = True
  isEmpty _ = False

  push a (MkHeap tree ins) = MkHeap (insert a tree) (a :: ins)

  peek (MkHeap tree _ ) = findMin tree

  pop (MkHeap tree ins) = let (tree', a) = popMin tree
                         in (MkHeap tree' ins, a)

  merge (MkHeap tree ins) (MkHeap tree' ins') = MkHeap (mergeTree tree tree') (ins ++ ins')

  stats (MkHeap _ ins) = Just $ show ins
