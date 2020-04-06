module Bautzen.Network

import Data.List

import Bautzen.SExp
import Bautzen.REPL.SExpParser

||| Returns a decimal string representation of `n` padded with 0s
||| The number must be representable with 6 digits or else it is
||| simply returned as a `String`
||| TODO: make it really total
export
padWith0 : Int -> String
padWith0 k =
  let num = show k
      len = prim__strLength num
  in if len < 6
       then  let padding = Prelude.pack $ replicate (fromInteger $ cast $ 6 - len) '0'
             in padding ++ num
       else num

||| Convert a `SExp` to "wire" format
export
toWire : SExp -> String
toWire sexp =
  let str = show sexp
      len = padWith0 (prim__strLength str)
  in  len ++ str

||| Convert a `String` to a SExp, if possible
export
fromWire : String -> Either String SExp
fromWire = parseSExp
