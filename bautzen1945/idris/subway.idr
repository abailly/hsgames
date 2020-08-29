module subway

data Subway : Type where
  MkSub : List Subway -> Subway

Eq Subway where
  MkSub ls == MkSub ls' = ls  == ls'

Ord Subway where
  compare (MkSub ls) (MkSub ls') = compare ls ls'

fromString : String -> Subway
fromString = ?fromChars . unpack
