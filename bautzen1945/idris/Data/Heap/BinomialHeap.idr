||| An implementation of `Data.Heap` based on binomial trees.
||| This implementation is an adaptation of Okasaki's `BinomialHeap`
||| found on pp.20-25 of his book.
||| SEe also:
||| * [BinomialHeap in Agda](https://github.com/ericpony/FLOLAC-2014/blob/master/Analysis%20and%20synthesis%20of%20inductive%20families/Examples/BinomialHeap.agda)
||| *  [BinomialHeap in Haskell](https://jaspervdj.be/posts/2018-09-04-binomial-heaps-101.html#binomial-heaps-lets-build-it-up)
module Data.Heap.BinomialHeap

import Data.Vect

%default total

mutual
  ||| A Binomial Tree
  ||| quoting Okasaki:
  ||| A binomial tree is inductively defined as:
  ||| * a binomial tree of rank 0 is a singleton node
  ||| * a binomial tree of rank (n+1) is formed by linking 2 binomial trees
  |||   of rank n, making one tree the leftmost child of the other
  |||
  ||| Equivalently, a binomial tree of rank `r` is a tree with
  ||| `r` children `t_1 .. t_r` where each `t_i` has rank `r - i`.
  data BinTree : (rank : Nat) -> Type -> Type where
    MkTree : (elem : a) -> (subs : Children rank a) -> BinTree rank a

  ||| A helper type for children of a binomial tree, ensuring
  ||| subtrees are stored in increasing rank order.
  data Children : (rank : Nat) -> Type -> Type where
    CNil : Children 0 a
    CCons : BinTree k a -> Children k a -> Children (S k) a

||| A singleton binomial tree
Singleton : a -> BinTree 0 a
Singleton x = MkTree x CNil

||| Basic operation to _grow_ binomial trees
||| Takes 2 trees of equal rank `r` and forms a new tree of rank `S r`
link : (Ord a) => (left : BinTree r a) -> (right : BinTree r a) -> BinTree (S r) a
link left@(MkTree elem subs) right@(MkTree elem' subs') =
  if elem <= elem'
  then MkTree elem (CCons right subs)
  else MkTree elem' (CCons left subs')
