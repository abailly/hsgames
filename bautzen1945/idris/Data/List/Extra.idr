module Data.List.Extra


||| Replicate the element `n` times
public export
replicate : (len : Nat) -> (x : elem) -> List elem
replicate Z _ = []
replicate (S k) x = x :: replicate k x

||| Find the first element of a list that satisfies a predicate, or `Nothing` if none do.
|||
||| Renamed to `find'` because Idris2 gets confused between Vect find and List find
public export
find' : (a -> Bool) -> List a -> Maybe a
find' p []      = Nothing
find' p (x::xs) =
  if p x then
    Just x
  else
    find' p xs
