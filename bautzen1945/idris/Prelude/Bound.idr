module Prelude.Bound

public export
interface Ord b => MinBound b where
  ||| The lower bound for the type
  minBound : b

public export
interface Ord b => MaxBound b where
  ||| The upper bound for the type
  maxBound : b
