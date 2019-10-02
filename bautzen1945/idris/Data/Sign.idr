module Data.Sign

||| A representation of signs for signed datatypes such as `ZZ`
public export
data Sign = Plus | Zero | Minus

export
opposite : Sign -> Sign
opposite Plus  = Minus
opposite Zero  = Zero
opposite Minus = Plus

export
multiply : Sign -> Sign -> Sign
multiply Zero  _    = Zero
multiply _     Zero = Zero
multiply Plus  x    = x
multiply Minus x    = opposite x

||| Discover the sign of some type
public export
interface Signed t where
  sign : t -> Sign

export
Signed Int where
  sign x with (compare x 0)
    sign x | LT = Minus
    sign x | EQ = Zero
    sign x | GT = Plus

export
Signed Integer where
  sign x with (compare x 0)
    sign x | LT = Minus
    sign x | EQ = Zero
    sign x | GT = Plus

export
Signed Double where
  sign x with (compare x 0.0)
    sign x | LT = Minus
    sign x | EQ = Zero
    sign x | GT = Plus
