module Data.Sign

%default total

||| A representation of signs for signed datatypes such as `ZZ`
public export
data Sign = Plus | Zero | Minus

public export
opposite : Sign -> Sign
opposite Plus  = Minus
opposite Zero  = Zero
opposite Minus = Plus

public export
multiply : Sign -> Sign -> Sign
multiply Zero  _    = Zero
multiply _     Zero = Zero
multiply Plus  x    = x
multiply Minus x    = opposite x

||| Discover the sign of some type
public export
interface Signed t where
  sign : t -> Sign

public export
Signed Int where
  sign x with (compare x 0)
    sign x | LT = Minus
    sign x | EQ = Zero
    sign x | GT = Plus

public export
Signed Integer where
  sign x with (compare x 0)
    sign x | LT = Minus
    sign x | EQ = Zero
    sign x | GT = Plus

public export
Signed Double where
  sign x with (compare x 0.0)
    sign x | LT = Minus
    sign x | EQ = Zero
    sign x | GT = Plus
