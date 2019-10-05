module Prelude.Either


||| Keep the payloads of all Left constructors in a list of Eithers
public export
lefts : List (Either a b) -> List a
lefts []      = []
lefts (x::xs) =
  case x of
    Left  l => l :: lefts xs
    Right r => lefts xs

||| Keep the payloads of all Right constructors in a list of Eithers
public export
rights : List (Either a b) -> List b
rights []      = []
rights (x::xs) =
  case x of
    Left  l => rights xs
    Right r => r :: rights xs


||| Split a list of Eithers into a list of the left elements and a list of the right elements
public export
partitionEithers : List (Either a b) -> (List a, List b)
partitionEithers l = (lefts l, rights l)
