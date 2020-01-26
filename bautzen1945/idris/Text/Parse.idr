||| Simple Parser combinators inspired by
||| http://dev.stephendiehl.com/fun/002_parsers.html
module Text.Parse

import public Text.Pos
import public Text.ParseError
import Data.List

public export
data Parser : (v : Type) -> Type where
  MkParser : (List Char -> List (v, List Char)) -> Parser v

parse : Parser v -> (List Char -> List (v, List Char))
parse (MkParser f) = f

export
runParser : (Show a) => Parser a -> String -> Either String a
runParser m s =
  case parse m (unpack s) of
    [(res, [])] => Right res
    [(_, rs)]   => Left "Parser did not consume entire stream."
    r           => Left $ "Parser error: " ++ show r

item : Parser Char
item = MkParser $ \s =>
  case s of
   []      => []
   (c::cs) => [(c,cs)]

bind : Parser a -> (a -> Parser b) -> Parser b
bind p f = MkParser $ \s => concatMap (\(a, s') => parse (f a) s') $ parse p s

unit : a -> Parser a
unit a = MkParser (\s => [(a,s)])

combine : Parser a -> Parser a -> Parser a
combine p q = MkParser (\s => parse p s ++ parse q s)

failure : Parser a
failure = MkParser (\cs => [])

option : Parser a -> Parser a -> Parser a
option  p q = MkParser $ \s =>
  case parse p s of
    []     => parse q s
    res    => res

public export
Functor Parser where
  map f (MkParser cs) = MkParser (\s => [(f a, b) | (a, b) <- cs s])

public export
Applicative Parser where
  pure = unit
  (MkParser cs1) <*> (MkParser cs2) = MkParser (\s => [(f a, s2) | (f, s1) <- cs1 s, (a, s2) <- cs2 s1])

public export
Monad Parser where
  (>>=)  = bind

public export
Alternative Parser where
  empty = failure
  (<|>) = option

-- -- Basic Combinators

-- | One or more.
some : Parser a -> Parser (List a)
some v = some_v
  where
    mutual
      many_v : Parser (List a)
      many_v = some_v <|> pure []

      some_v : Parser (List a)
      some_v = (::) <$> v <*> many_v

-- | Zero or more.
many : Parser a -> Parser (List a)
many v = many_v
  where
    mutual
      many_v : Parser (List a)
      many_v = some_v <|> pure []

      some_v : Parser (List a)
      some_v = (::) <$> v <*> many_v

satisfy : (Char -> Bool) -> Parser Char
satisfy p = item `bind` \c =>
  if p c
  then unit c
  else (MkParser (\cs => []))

export
notFollowedBy : Parser v -> Parser ()
notFollowedBy (MkParser f) = MkParser $ \ s =>
  case f s of
    [(_,[])] => []
    _ => [((),[])]

export
between : Parser a -> Parser b -> Parser v -> Parser v
between start end content = start *> content <* end

export
optional : Parser v -> Parser (Maybe v)
optional p =
  (do v <- p; pure (Just v)) <|> pure Nothing

export
many1 : Parser v -> Parser (List v)
many1 p = do { v <- p; vs <- many p; pure (v :: vs) }

export
sepBy1 : Parser v -> Parser vsep -> Parser (List v)
sepBy1 p psep = do v <- p
                   vs <- many (do { psep; p })
                   pure (v :: vs)

export
sepBy : Parser v -> Parser vsep -> Parser (List v)
sepBy p psep = sepBy1 p psep <|> pure []

export
endBy : Parser v -> Parser vend -> Parser (List v)
endBy p pend = many (do { v <- p; pend; pure v })

export
endBy1 : Parser v -> Parser vend -> Parser (List v)
endBy1 p pend = many1 (do { v <- p; pend; pure v })

export
sepEndBy1 : Parser v -> Parser vsep -> Parser (List v)
sepEndBy1 p psep = do v <- sepBy1 p psep; optional psep; pure v

export
sepEndBy : Parser v -> Parser vsep -> Parser (List v)
sepEndBy p psep = do v <- sepBy p psep; optional psep; pure v

chainl1 : Parser v -> Parser (v -> v -> v) -> Parser v
chainl1 p psep =
  let psuffix : v -> Parser v
      psuffix z = (do { f <- psep; v <- p ;  psuffix (f z v)})
                  <|> pure z
  in do v <- p
        psuffix v

chainl : Parser v -> Parser (v -> v -> v) -> v -> Parser v
chainl p psep z = chainl1 p psep <|> pure z

chainr1 : Parser v -> Parser (v -> v -> v) -> Parser v
chainr1 p psep = (do { v <- p ; f <- psep ; w <- chainr1 p psep ; pure (f v w) })
                 <|> p

chainr : Parser v -> Parser (v -> v -> v) -> v -> Parser v
chainr p psep z = chainr1 p psep <|> pure z

choice : List (Parser v) -> Parser v
choice [p] = p
choice (p :: ps) = p <|> choice ps


-- Character-oriented parsers

export
anyChar : Parser Char
anyChar = item

export
char : Char -> Parser Char
char ch = satisfy (\c => c == ch)

export
oneOf : List Char -> Parser Char
oneOf chs = satisfy (\c => c `elem` chs)

export
noneOf : List Char -> Parser Char
noneOf chs = satisfy (\c => not (c `elem` chs))

export
string : List Char -> Parser (List Char)
string str = p str
  where p : List Char -> Parser (List Char)
        p [] = pure str
        p (ch :: chs) = do
          char ch
          p chs

export
stringFrom : List (List Char) -> Parser (List Char)
stringFrom [str] = string str
stringFrom (str :: strs) = string str <|> stringFrom strs

export
upper : Parser Char
upper = satisfy isUpper

export
lower : Parser Char
lower = satisfy isLower

export
letter : Parser Char
letter = satisfy isAlpha

export
alphaNum : Parser Char
alphaNum = satisfy isAlphaNum

export
digit : Parser Char
digit = satisfy isDigit

export
hexDigit : Parser Char
hexDigit = satisfy isHexDigit

export
octDigit : Parser Char
octDigit = satisfy isOctDigit

export
newline : Parser Char
newline = char '\n'

export
tab : Parser Char
tab = char '\t'

export
space : Parser Char
space = satisfy isSpace

export
spaces : Parser (List Char)
spaces = many space

export
eof : Parser ()
eof = notFollowedBy anyChar
