||| Packrat monadic parser combinators library
||| Directly translated from http://bford.info/pub/lang/packrat-icfp02/
module Text.Parse

import public Text.Pos
import public Text.ParseError
import Data.List

-- Data types
public export
data Result : (v : Type) -> (d : Type) -> Type where
     Parsed : v -> d -> ParseError -> Result v d
     NoParse : ParseError -> Result v d

public export
data Parser : (d : Type) -> (v : Type) -> Type where
  MkParser : (d -> Lazy (Result v d)) -> Parser d v

public export
interface Derivs d where
  dvPos  : d -> Pos String
  dvChar : d -> Lazy (Result Char d)

export
nullError : Derivs d => d -> ParseError
nullError dvs = MkParseError (dvPos dvs) []

export
expError : Derivs d => d  -> String -> ParseError
expError dvs desc = MkParseError (dvPos dvs) [Expected desc]

export
eofError : (Derivs d) => d -> ParseError
eofError dvs = MkParseError (dvPos dvs) [ Msg "end of input" ]

export
expected : Derivs d => String -> Parser d v
expected desc = MkParser (\dvs => NoParse (expError dvs desc))

export
failAt : Derivs d => Pos String -> String -> Parser d v
failAt pos msg = MkParser (\dvs => NoParse (msgError pos msg))

-- -- Basic Combinators

infixl 1 <?>
infixl 1 <?!>

export
implementation (Derivs d) => Functor (Parser d) where
  map f (MkParser p) = MkParser $ \ v => case p v of
                                           Parsed v d err => Parsed (f v) d err
                                           NoParse err => NoParse err

export
implementation Derivs d => Applicative (Parser d) where
  pure x = MkParser (\dvs => Parsed x dvs (nullError dvs))

  (MkParser pf) <*> (MkParser pa) =
    MkParser (\ dvs =>
               case pf dvs of
                 Parsed f rem err  =>
                   case pa rem of
                     Parsed x rem' err' => Parsed (f x) rem' (joinErrors err err')
                     NoParse errs => NoParse errs
                 NoParse errs => NoParse errs)

export
implementation Derivs d => Monad (Parser d) where
  (MkParser p1) >>= f = MkParser parse
    where
      second : ParseError -> Result b d -> Result b d
      second err1 (Parsed val rem err) =
             Parsed val rem (joinErrors err1 err)
      second err1 (NoParse err) =
             NoParse (joinErrors err1 err)

      first : Result a d -> Lazy (Result b d)
      first (Parsed val rem err) =
        let MkParser p2 = f val
        in second err (p2 rem)
      first (NoParse err) = NoParse err

      parse : d -> Lazy (Result b d)
      parse dvs = first (p1 dvs)


export
fail : Derivs d => String -> Parser d v
fail msg = MkParser (\dvs => NoParse (msgError (dvPos dvs) msg))

export
unexpected : Derivs d => String -> Parser d v
unexpected str = fail ("unexpected " ++ str)

export
implementation Derivs d => Alternative (Parser d) where
  empty = MkParser $ \ dv => NoParse (MkParseError (dvPos dv) [ Msg "Nothing to parse" ])

  (MkParser p1) <|> (MkParser p2) = MkParser parse
      where second : ParseError -> Result a d -> Lazy (Result a d)
            second err1 (Parsed val rem err) =
                   Parsed val rem (joinErrors err1 err)
            second err1 (NoParse err) =
                   NoParse (joinErrors err1 err)

            first : d -> Result a d -> Lazy (Result a d)
            first dvs result@(Parsed x y z) = result
            first dvs (NoParse err) = second err (p2 dvs)

            parse : d -> Lazy (Result a d)
            parse dvs = first dvs (p1 dvs)


export
satisfy : Derivs d => Parser d v -> (v -> Bool) -> Parser d v
satisfy (MkParser p) test = MkParser parse
    where
      check : d -> Result v d -> Lazy (Result v d)
      check dvs (result @ (Parsed val rem err)) =
        if test val
         then result
        else NoParse (expError dvs "a character satisfying predicate")
      check dvs none = none

      parse : d -> Lazy (Result v d)
      parse dvs = check dvs (p dvs)

export
notFollowedBy : (Derivs d, Show v) => Parser d v -> Parser d ()
notFollowedBy (MkParser p) = MkParser parse
  where
    parse : (Derivs d, Show v) => d -> Lazy (Result () d)
    parse dvs = case (p dvs) of
      Parsed val rem err => NoParse (expError dvs $ "unexpected " ++ show val)
      NoParse err => Parsed () dvs (nullError dvs)

export
between : Derivs d => Parser d a -> Parser d b -> Parser d v -> Parser d v
between start end content = start *> content <* end

export
optional : Derivs d => Parser d v -> Parser d (Maybe v)
optional p =
  (do v <- p; pure (Just v)) <|> pure Nothing

export
many : Derivs d => Parser d v -> Parser d (List v)
many p =
  (do { v <- p; vs <- many p; pure (v :: vs) } ) <|> pure []

export
many1 : Derivs d => Parser d v -> Parser d (List v)
many1 p = do { v <- p; vs <- many p; pure (v :: vs) }

export
sepBy1 : Derivs d => Parser d v -> Parser d vsep -> Parser d (List v)
sepBy1 p psep = do v <- p
                   vs <- many (do { psep; p })
                   pure (v :: vs)

export
sepBy : Derivs d => Parser d v -> Parser d vsep -> Parser d (List v)
sepBy p psep = sepBy1 p psep <|> pure []

export
endBy : Derivs d => Parser d v -> Parser d vend -> Parser d (List v)
endBy p pend = many (do { v <- p; pend; pure v })

export
endBy1 : Derivs d => Parser d v -> Parser d vend -> Parser d (List v)
endBy1 p pend = many1 (do { v <- p; pend; pure v })

export
sepEndBy1 : Derivs d => Parser d v -> Parser d vsep -> Parser d (List v)
sepEndBy1 p psep = do v <- sepBy1 p psep; optional psep; pure v

export
sepEndBy : Derivs d => Parser d v -> Parser d vsep -> Parser d (List v)
sepEndBy p psep = do v <- sepBy p psep; optional psep; pure v

-- -- chainl1 :: Derivs d => Parser d v -> Parser d (v -> v -> v) -> Parser d v
-- -- chainl1 p psep =
-- --   let psuffix z = (do f <- psep
-- --           v <- p
-- --           psuffix (f z v))
-- --       <|> return z
-- --   in do v <- p
-- --         psuffix v

-- -- chainl :: Derivs d => Parser d v -> Parser d (v -> v -> v) -> v -> Parser d v
-- -- chainl p psep z = chainl1 p psep <|> return z

-- -- chainr1 :: Derivs d => Parser d v -> Parser d (v -> v -> v) -> Parser d v
-- -- chainr1 p psep = (do v <- p
-- --          f <- psep
-- --          w <- chainr1 p psep
-- --          return (f v w))
-- --      <|> p

-- -- chainr :: Derivs d => Parser d v -> Parser d (v -> v -> v) -> v -> Parser d v
-- -- chainr p psep z = chainr1 p psep <|> return z

-- -- choice :: Derivs d => [Parser d v] -> Parser d v
-- -- choice [p] = p
-- -- choice (p:ps) = p <|> choice ps


-- -- -- Error handling

||| Annotate a parser with a description of the construct to be parsed.
||| The resulting parser yields an "expected" error message
||| if the construct cannot be parsed
||| and if no error information is already available
||| indicating a position farther right in the source code
||| (which would normally be more localized/detailed information).
export
(<?>) : (Derivs d, Show v) => Parser d v -> String -> Parser d v
(MkParser p) <?> desc = MkParser (\ dvs => munge dvs (p dvs))
  where
    fix : d -> ParseError -> ParseError
    fix dvs err@(MkParseError pos ms) =
      if pos > dvPos dvs
      then err
      else expError dvs desc

    munge : (Derivs d, Show v) => d -> Result v d -> Result v d
    munge dvs (Parsed v rem err) = Parsed v rem (fix dvs err)
    munge dvs (NoParse err) =  NoParse (fix dvs err)

||| Stronger version of the <?> error annotation operator.
||| which unconditionally overrides any existing error information.
export
(<?!>) : (Derivs d, Show v) => Parser d v -> String -> Parser d v
(MkParser p) <?!> desc = MkParser (\dvs => munge dvs (p dvs))
  where
    fix : d -> ParseError -> ParseError
    fix dvs (err @ (MkParseError p ms)) = expError dvs desc

    munge : (Derivs d, Show v) => d -> Result v d -> Result v d
    munge dvs (Parsed v rem err) = Parsed v rem (fix dvs err)
    munge dvs (NoParse err) = NoParse (fix dvs err)


-- Character-oriented parsers

export
anyChar : Derivs d => Parser d Char
anyChar = MkParser dvChar

export
char : Derivs d => Char -> Parser d Char
char ch = satisfy anyChar (\c => c == ch) <?> show ch

export
oneOf : Derivs d => List Char -> Parser d Char
oneOf chs = satisfy anyChar (\c => c `elem` chs)
      <?> ("one of the characters " ++ pack chs)

export
noneOf : Derivs d => List Char -> Parser d Char
noneOf chs = satisfy anyChar (\c => not (c `elem` chs))
       <?> ("any character not in " ++ pack chs)

export
string : Derivs d => String -> Parser d String
string str = p (unpack str) <?> show str
  where p : (Derivs d) => List Char -> Parser d String
        p [] = pure str
        p (ch :: chs) = do
          char ch
          p chs

export
stringFrom : Derivs d => List String -> Parser d String
stringFrom [str] = string str
stringFrom (str :: strs) = string str <|> stringFrom strs

export
upper : Derivs d => Parser d Char
upper = satisfy anyChar isUpper <?> "uppercase letter"

export
lower : Derivs d => Parser d Char
lower = satisfy anyChar isLower <?> "lowercase letter"

export
letter : Derivs d => Parser d Char
letter = satisfy anyChar isAlpha <?> "letter"

export
alphaNum : Derivs d => Parser d Char
alphaNum = satisfy anyChar isAlphaNum <?> "letter or digit"

export
digit : Derivs d => Parser d Char
digit = satisfy anyChar isDigit -- <?> "digit"

export
hexDigit : Derivs d => Parser d Char
hexDigit = satisfy anyChar isHexDigit <?> "hexadecimal digit (0-9, a-f)"

export
octDigit :  Derivs d => Parser d Char
octDigit = satisfy anyChar isOctDigit <?> "octal digit (0-7)"

export
newline :  Derivs d => Parser d Char
newline = char '\n'

export
tab : Derivs d => Parser d Char
tab = char '\t'

export
space : Derivs d => Parser d Char
space = satisfy anyChar isSpace <?> "whitespace character"

export
spaces : Derivs d => Parser d (List Char)
spaces = many space

export
eof : Derivs d => Parser d ()
eof = notFollowedBy anyChar <?> "end of input"


-- -- State manipulation

-- getDerivs :: Derivs d => Parser d d
-- getDerivs = Parser (\dvs -> Parsed dvs dvs (nullError dvs))

-- setDerivs :: Derivs d => d -> Parser d ()
-- setDerivs newdvs = Parser (\dvs -> Parsed () newdvs (nullError dvs))

-- getPos :: Derivs d => Parser d Pos
-- getPos = Parser (\dvs -> Parsed (dvPos dvs) dvs (nullError dvs))


-- -- Special function that converts a Derivs "back" into an ordinary String
-- -- by extracting the successive dvChar elements.
-- dvString :: Derivs d => d -> String
-- dvString d =
--   case dvChar d of
--     NoParse err -> []
--     Parsed c rem err -> (c : dvString rem)
