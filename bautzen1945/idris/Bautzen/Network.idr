module Bautzen.Network

import Bautzen.Id
import Data.List
import Language.JSON

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

||| Convert a `String` to "wire" format
export
toWire : String -> String
toWire str =
  let len = padWith0 (prim__strLength str)
  in  len ++ str

public
export
data Handshake =
   Connect Id
   | Connected

export
Cast Handshake JSON where
  cast (Connect k) = JObject [( "tag", JString "Connect"), ("playerKey", cast k)]
  cast Connected = JObject [( "tag", JString "Connected")]
