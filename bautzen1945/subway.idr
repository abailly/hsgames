module subway

%default partial

data Subway : Type where
  Sub : List Subway -> Subway

Eq Subway where
  (Sub ls) == (Sub ls') = ls  == ls'

Ord Subway where
  compare (Sub ls) (Sub ls') = compare ls ls'

fromChars : Subway -> List Subway -> List Char -> Subway
fromChars _ [root] [] = root
fromChars cur@(Sub ys) stack ('0' :: xs) =
  let newSub = Sub []
  in fromChars newSub (Sub (newSub :: ys) :: stack) xs
fromChars cur@(Sub ys) (sub :: subs) ('1' :: xs) =
  fromChars sub subs xs

fromString : String -> Subway
fromString = fromChars (Sub []) [] . unpack

canonical : Subway -> Subway
canonical (Sub ss) = Sub $ sort (map canonical ss)

-- traverseSub : Subway -> List Char
-- traverseSub sub = traverseSub' sub []
--   where
--     traverseSub' : Subway -> Maybe Subway -> List Char -> List Char
--     traverseSub' (Sub [])
-- toString : Subway -> String
-- toString  = pack . traverseSub . canonical
