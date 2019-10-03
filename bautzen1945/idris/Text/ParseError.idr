module Text.ParseError

import Text.Pos

public export
data Message : Type where
  Expected : String -> Message
  Msg : String -> Message

public export
Eq Message where
  (Expected e1) == (Expected e2) = e1 == e2
  (Msg m1)      == (Msg m2)      = m1 == m2
  _             == _             = False


public export
record ParseError  where
  constructor MkParseError
  errorPos : Pos String -- TODO generalise source
  errorMessages  : List Message

public export
Show ParseError where
  show (MkParseError pos []) =
    show pos ++ ": unknown error"
  show (MkParseError pos msgs) = expectmsg expects ++ messages msgs
     where
       mutual
         expects : List String
         expects = getExpects msgs

         getExpects : List Message -> List String
         getExpects [] = []
         getExpects (Expected exp :: rest) = exp :: getExpects rest
         getExpects (Msg msg :: rest) = getExpects rest

         expectmsg : List String -> String
         expectmsg [] = ""
         expectmsg [exp] = show pos ++ ": expecting " ++ exp ++ "\n"
         expectmsg [e1, e2] = show pos ++ ": expecting either "
               ++ e1 ++ " or " ++ e2 ++ "\n"
         expectmsg (first :: rest) = show pos ++ ": expecting one of: "
                 ++ first ++ expectlist rest
                 ++ "\n"

         expectlist : List String -> String
         expectlist [last] = ", or " ++ last
         expectlist (mid :: rest) = ", " ++ mid ++ expectlist rest

         messages : List Message -> String
         messages [] = ""
         messages (Expected exp :: rest) = messages rest
         messages (Msg msg :: rest) =
           show pos ++ ": " ++ msg ++ "\n" ++ messages rest

-- Comparison operators for ParseError just compare relative positions.
public export
implementation Eq ParseError where
  (MkParseError p1 m1) == (MkParseError p2 m2)  = p1 == p2
  (MkParseError p1 m1) /= (MkParseError p2 m2)  = p1 /= p2

||| Potentially join two sets of ParseErrors,
||| but only if the position didn't change from the first to the second.
||| If it did, just return the "new" (second) set of errors.
public export
joinErrors : ParseError -> ParseError -> ParseError
joinErrors (MkParseError p m) (MkParseError p' m') =
  if p' > p || (m == [])
  then MkParseError p' m'
    else if p > p' || (m' == [])
         then MkParseError p m
         else MkParseError p (m ++ m')

public export
msgError : Pos String -> String -> ParseError
msgError pos msg = MkParseError pos [Msg msg]
