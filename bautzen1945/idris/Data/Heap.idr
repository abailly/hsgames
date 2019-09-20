||| Heap (aka. Priority Queue) data structure.
|||
||| A _Heap_ provides an efficient way to store a bunch of `Ord`ered
||| elements in such a way that it's easy to retrieve the minimum
||| element. It's a basic data structure to implement classical graph
||| algorithms like [A*](https://en.wikipedia.org/wiki/A*_search_algorithm).
|||
||| We provide a single implementation called _Binomial Heaps_ from
||| Okasaki's [Purely Functional Data Structures](https://www.cs.cmu.edu/~rwh/theses/okasaki.pdf)
||| but the interface can of course be easily implemented by other
||| data structures
module Data.Heap

%access public export

||| Interface to Heap/Priority Queue
interface Heap (heap : Type -> Type) where

  ||| Create a new, empty `Heap`
  empty : heap a

  ||| Test whether or not given `heap` is empty
  isEmpty : (q : heap a) -> Bool

  ||| Insert given element into the heap
  ||| @elem the element to insert. Must implement `Ord` interface
  ||| @q the heap to insert element into
  ||| @return a new heap with element inserted. If `elem` is the
  ||| smallest element in the heap, then it's inserted at its top
  push : (Ord a) => (elem : a) -> (q : heap a) -> heap a

  ||| Try to find the mininal element of the heap
  ||| @q the heap to peek at
  ||| @return Nothing if the heap is empty, or the minimal element
  ||| otherwise.
  peek : (Ord a) => (q : heap a) -> Maybe a

  ||| Try to pop the minimal element off the heap.
  ||| @q the heap to pop from
  ||| @return the updated heap and the minimal element. If the
  ||| second element of the pair is Nothing then the returned
  ||| heap is the unmodified `q`
  pop : (Ord a) => (q : heap a) -> (heap a, Maybe a)

  ||| Merge to heaps, restructuring their elements to respect
  ||| the ordering.
  ||| @q first heap to merge
  ||| @q' second heap to merge
  ||| @return a new heap containing all elements of `q` and `q'`
  ||| in correct order
  merge : (Ord a) => (q : heap a) -> (q' : heap a) -> heap a

  ||| Extract statistics form the `heap` for debugging purpose
  |||
  ||| This interface is kind of uninteresting and is just a stop-gap solution,
  ||| it needs to be refactored to produce some real statistics data, like the
  ||| number of operations done so far on this heap and possibly their timing.
  stats : (Show a) => heap a -> Maybe String
