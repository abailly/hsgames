||| SExpParser stolen from Idris2 sources' `Idris.IDEMode.Parser`.
module Bautzen.REPL.SExpParser

import Bautzen.SExp

import Text.Parser
import Text.Lexer
import Text.Token
import Data.Strings
import Data.List

%hide Text.Lexer.Core.strTail

public export
data SExpKind = Ident
               | Literal
               | StrLit
               | Symbol
               | Comment
               | EndInput

Show a => Show (Token a) where
  show (Tok k t) = "Tok " ++ show k ++ "  " ++ t

Show SExpKind where
 show Ident = "Ident"
 show  Literal = "Literal"
 show  StrLit = "String"
 show  Symbol = "Symbol"
 show  Comment = "Comment"
 show  EndInput = "EOF"

TokenKind SExpKind where
  TokType Ident = String
  TokType Literal = Integer
  TokType StrLit = String
  TokType Symbol = String
  TokType Comment = ()
  TokType EndInput = ()

  tokValue Ident x = x
  tokValue StrLit x = x
  tokValue Literal x = cast x
  tokValue Symbol x = x
  tokValue Comment x = ()
  tokValue EndInput x = ()


public export
data ParseError = ParseFail String (Maybe (Int, Int)) (List (Token SExpKind))
                | LexFail (Int, Int, String)
                | LitFail (List Int)

export
Show ParseError where
  show (ParseFail err loc toks)
      = "Parse error: " ++ err ++ " (next tokens: "
            ++ show (take 10 toks) ++ ")"
  show (LexFail (c, l, str))
      = "Lex error at " ++ show (c, l) ++ " input: " ++ str
  show (LitFail l)
      = "Lit error(s) at " ++ show l

symbols : List String
symbols = ["(", ":", ")"]

ident : Lexer
ident = pred startIdent <+> many (pred validIdent)
  where
    startIdent : Char -> Bool
    startIdent '_' = True
    startIdent x = isAlpha x

    validIdent : Char -> Bool
    validIdent '_' = True
    validIdent '-' = True
    validIdent '\'' = True
    validIdent '?' = True
    validIdent '!' = True
    validIdent x = isAlphaNum x

ideTokens : TokenMap (Token SExpKind)
ideTokens =
  map (\x => (exact x, \s => Tok Symbol x)) symbols ++
    [ (digits, \x => Tok Literal x)
    , (stringLit, \x => Tok StrLit (stripQuotes x))
    , (ident, \x => Tok Ident x)
    , (space, \x => Tok Comment x)
    ]
  where
    stripQuotes : String -> String
    -- ASSUMPTION! Only total because we know we're getting quoted strings.
    stripQuotes = assert_total (strTail . reverse . strTail . reverse)

idelex : String -> Either (Int, Int, String) (List (TokenData (Token SExpKind)))
idelex str
    = case lex ideTokens str of
           -- Add the EndInput token so that we'll have a line and column
           -- number to read when storing spans in the file
           (tok, (l, c, "")) => Right (filter notComment tok ++
                                      [MkToken l c (Tok EndInput "")])
           (_, fail) => Left fail
    where
      notComment : TokenData (Token SExpKind) -> Bool
      notComment t = case tok t of
                          Tok Comment _ => False
                          _ => True

Rule : Type -> Type
Rule ty = Grammar (TokenData (Token SExpKind)) True ty

EmptyRule : Type -> Type
EmptyRule ty = Grammar (TokenData (Token SExpKind)) False ty

eoi : EmptyRule ()
eoi
    = do _ <- nextIs "Expected end of input" (isEOI . tok)
         pure ()
  where
    isEOI : Token SExpKind -> Bool
    isEOI (Tok EndInput _) = True
    isEOI _ = False

intLit : Rule Integer
intLit
    = terminal "Expected integer literal"
               (\x => case tok x of
                           Tok Literal i => Just (cast i)
                           _ => Nothing)

strLit : Rule String
strLit
    = terminal "Expected string literal"
               (\x => case tok x of
                           Tok StrLit s => Just s
                           _ => Nothing)


symbol : String -> Rule ()
symbol req
    = terminal ("Expected '" ++ req ++ "'")
               (\x => case tok x of
                           Tok Symbol s => if s == req
                                           then Just ()
                                           else Nothing
                           _ => Nothing)

exactIdent : String -> Rule ()
exactIdent req
    = terminal ("Expected " ++ req)
               (\x => case tok x of
                           Tok Ident s => if s == req
                                          then Just ()
                                          else Nothing
                           _ => Nothing)

identPart : Rule String
identPart
    = terminal "Expected name"
               (\x => case tok x of
                           Tok Ident str => Just str
                           _ => Nothing)

sexp : Rule SExp
sexp
    = do i <- fromInteger <$> intLit
         pure (SInt i)
  <|> do str <- strLit
         pure (SStr str)
  <|> do symbol ":"; x <- identPart
         pure (SSym x)
  <|> do symbol "("
         xs <- many sexp
         symbol ")"
         pure (SList xs)

ideParser : String -> Grammar (TokenData (Token SExpKind)) True ty -> Either String ty
ideParser str p
    = case idelex str of
           Left err => Left $ show $ LexFail err
           Right toks =>
              case parse p toks of
                   Left (Error err []) =>
                          Left $ show $ ParseFail err Nothing []
                   Left (Error err (t :: ts)) =>
                          Left $ show $ ParseFail err (Just (line t, col t))
                                                  (map tok (t :: ts))
                   Right (val, _) => Right val

export
parseSExp : String -> Either String SExp
parseSExp inp
    = ideParser inp (do c <- sexp; eoi; pure c)
