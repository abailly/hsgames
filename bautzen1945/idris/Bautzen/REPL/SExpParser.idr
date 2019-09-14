module Bautzen.REPL.SExpParser

import Bautzen.SExp

import Text.Parse
import Text.ParseError

record SExpParse where
  constructor MkSExpParse
  sProg : Lazy (Result (List SExp) SExpParse)
  sList : Lazy (Result SExp SExpParse)
  sSym : Lazy (Result SExp SExpParse)
  sStr : Lazy (Result SExp SExpParse)
  sInt : Lazy (Result SExp SExpParse)
  sChar : Lazy (Result Char SExpParse)
  sPos : Pos String

runP : d -> Parser d v -> Lazy (Result v d)
runP d (MkParser p) = p d


pDecimal : (Derivs d) => Parser d Int
pDecimal = foldl (\ x, c => x * 10 + (cast c - 48)) 0 <$> many1 digit <?> "decimal integer"

Derivs SExpParse where
  dvChar d = sChar d
  dvPos d = sPos d

pSInt : Parser SExpParse SExp
pSInt = SInt <$> pDecimal

pSSym : Parser SExpParse SExp
pSSym = do
  c <- letter <|> oneOf (unpack ":*%&$#@!><?/[]{}~-+=_")
  cs <- many (alphaNum <|> oneOf (unpack ":*%&$#@!><?/[]{}~-+=_"))
  pure $ SSym $ pack $ c :: cs

pSStr : Parser SExpParse SExp
pSStr = SStr . pack <$> between (char '"') (char '"') (many $ noneOf ['"'] )

pSExpList : Parser SExpParse SExp
pSExpList =
  SList <$> between (char '(' <* spaces) (char ')') (sepBy (MkParser sList <|> MkParser sSym <|> MkParser sStr <|> MkParser sInt) spaces)

pProg : Parser SExpParse (List SExp)
pProg = many (MkParser sList) <* eof

sexpParser : Pos String -> List Char -> SExpParse
sexpParser pos s = d
  where
    mutual
      d : SExpParse
      d = MkSExpParse
          (runP d pProg)
          (runP d pSExpList)
          (runP d pSSym)
          (runP d pSStr)
          (runP d pSInt)
          chr
          pos

      chr : Result Char SExpParse
      chr  = case s of
               (c :: s') => Parsed c (sexpParser (nextPos pos c) s') (nullError d)
               [] => NoParse (eofError d)

export
parseSExp : String -> Either String SExp
parseSExp s = case sList (sexpParser (MkPos "<input>" 1 1) $ unpack s) of
                  Parsed v d' e' => Right v
                  NoParse err => Left $ show err

parseProg : String -> Either String (List SExp)
parseProg s = case sProg (sexpParser (MkPos "<input>" 1 1) $ unpack s) of
                  Parsed v d' e' => Right v
                  NoParse err => Left $ show err
