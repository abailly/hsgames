module Text.ParseTest

import Text.Parse
import Text.ParseError
import Text.Pos
import System


-- * Example

record Arithmetics where
  constructor MkArith
  arMulti : Lazy (Result Int Arithmetics)
  arDecimal : Lazy (Result Int Arithmetics)
  arChar : Lazy (Result Char Arithmetics)
  arPos : Pos String

Derivs Arithmetics where
  dvChar d = arChar d
  dvPos d = arPos d

pDecimal : (Derivs d) => Parser d Int
pDecimal = foldl (\ x, c => x * 10 + (cast c - 48)) 0 <$> many1 digit <?> "decimal integer"

multSuffix : Parser Arithmetics (Int -> Int)
multSuffix = (do spaces *> char '*' <* spaces
                 (*) <$> (MkParser arDecimal)) <|> pure (*1)

pMult : Parser Arithmetics Int
pMult = do
  x <- MkParser arDecimal
  f <- multSuffix
  pure $ f x

runP : d -> Parser d v -> Lazy (Result v d)
runP d (MkParser p) = p d

parseAr : Pos String -> List Char -> Arithmetics
parseAr pos s = d
  where
    mutual
      d : Arithmetics
      d = MkArith
          (runP d pMult)
          (runP d pDecimal)
          chr
          pos

      chr : Result Char Arithmetics
      chr  = case s of
               (c :: s') => Parsed c (parseAr (nextPos pos c) s') (nullError d)
               [] => NoParse (eofError d)

eval : String -> Either String Int
eval s = case arMulti (parseAr (MkPos "<input>" 1 1) $ unpack s) of
              Parsed v d' e' => Right v
              NoParse err => Left $ show err

-- * Tests

shouldBe : (Eq a, Show a) => a -> a -> IO (Either String ())
shouldBe a a' =
  if a /= a'
  then pure $ Left ("got " ++ show a ++ ", expected " ++ show a')
  else pure $ Right ()

test_canParseASingleDigitString : IO (Either String ())
test_canParseASingleDigitString = do
  let res = eval "1"
  res `shouldBe` Right 1

test_canParseAMultipleDigitString : IO (Either String ())
test_canParseAMultipleDigitString = do
  let res = eval "13"
  res `shouldBe` Right 13

test_canParseAmultiplicationOperation : IO (Either String ())
test_canParseAmultiplicationOperation = do
  let res = eval "10 * 12"
  res `shouldBe` Right 120

data SExp : Type where
  SList : List SExp -> SExp
  SSym : String -> SExp
  SInt : Int -> SExp

Show SExp where
  show (SList xs) = "(" ++ concat (intersperse " " $ map show xs) ++ ")"
  show (SSym x)   = x
  show (SInt x)   = show x

Eq SExp where
  (SList []) == (SList []) = True
  (SList (x :: xs)) == (SList (x' :: xs')) = x == x' && equal xs xs'
    where
      equal : List SExp -> List SExp -> Bool
      equal [] [] = True
      equal (x :: xs) (y :: ys) = x == y && equal xs ys
      equal _ _ = False

  (SSym x) == (SSym x') = x == x'
  (SInt x) == (SInt x') = x == x'
  _ == _ = True

record SExpParse where
  constructor MkSExpParse
  sProg : Lazy (Result (List SExp) SExpParse)
  sList : Lazy (Result SExp SExpParse)
  sSym : Lazy (Result SExp SExpParse)
  sInt : Lazy (Result SExp SExpParse)
  sChar : Lazy (Result Char SExpParse)
  sPos : Pos String

Derivs SExpParse where
  dvChar d = sChar d
  dvPos d = sPos d

pSInt : Parser SExpParse SExp
pSInt = SInt <$> pDecimal

pSSym : Parser SExpParse SExp
pSSym = do
  c <- letter
  cs <- many alphaNum
  pure $ SSym $ pack $ c :: cs

pSExpList : Parser SExpParse SExp
pSExpList =
  SList <$> between (char '(' <* spaces) (char ')') (sepBy (MkParser sList <|> MkParser sSym <|> MkParser sInt) spaces)

pProg : Parser SExpParse (List SExp)
pProg = many (MkParser sList) <* eof

parseSExp : Pos String -> List Char -> SExpParse
parseSExp pos s = d
  where
    mutual
      d : SExpParse
      d = MkSExpParse
          (runP d pProg)
          (runP d pSExpList)
          (runP d pSSym)
          (runP d pSInt)
          chr
          pos

      chr : Result Char SExpParse
      chr  = case s of
               (c :: s') => Parsed c (parseSExp (nextPos pos c) s') (nullError d)
               [] => NoParse (eofError d)

evalSExp : String -> Either String (List SExp)
evalSExp s = case sProg (parseSExp (MkPos "<input>" 1 1) $ unpack s) of
                  Parsed v d' e' => Right v
                  NoParse err => Left $ show err


test_canParseTokensInaSexp : IO (Either String ())
test_canParseTokensInaSexp = do
  let res = evalSExp "(foo bar 12)"
  res `shouldBe` Right [(SList [ SSym "foo", SSym "bar", SInt 12 ])]

  let res1 = evalSExp "(foo bar \n (quux) \n\n12)"
  res1 `shouldBe` Right [(SList [ SSym "foo", SSym "bar", SList [ SSym "quux"], SInt 12 ])]

test_returnsAnError : IO (Either String ())
test_returnsAnError = do
  eval "z" `shouldBe` Left "<input>:1:1: expecting decimal integer\n"

test_parseNotFollowedBy : IO (Either String ())
test_parseNotFollowedBy = do
  evalSExp "(foo)  " `shouldBe` Left "<input>:1:6: expecting end of input\n"

export
test : IO ()
test = do
  (lefts, _) <- partitionEithers <$> sequence [ test_canParseASingleDigitString
                                              , test_canParseAMultipleDigitString
                                              , test_canParseAmultiplicationOperation
                                              , test_canParseTokensInaSexp
                                              , test_parseNotFollowedBy
                                              , test_returnsAnError
                                              ]
  when (lefts /= []) $ do
    traverse putStrLn lefts
    exit 1
