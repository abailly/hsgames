module Bautzen.Network

import Bautzen.Id
import Bautzen.Options
import Data.List
import Data.Strings.Extra
import Language.JSON

import Network.Socket

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

public
export
Logger : Type
Logger = String -> IO ()

export
mkLogger : Verbosity -> Logger
mkLogger Quiet _ = pure ()
mkLogger (Verbose name) msg =
  putStrLn  $ "[" ++ name ++ "] " ++ msg


export
recvAll : Socket -> ByteLength -> IO (Either String String)
recvAll socket len = go "" len
  where
    go : String -> ByteLength -> IO (Either String String)
    go acc remaining = do
      Right (str, res) <- recv socket remaining
        | Left err => pure $ Left ("failed to receive length of message " ++ show err)
      if res == remaining
       then pure $ Right (acc ++ str)
       else go (acc ++ str) (remaining - res)

export
receive : Logger -> Socket -> IO (Either String String)
receive log socket = do
  log "waiting for input"
  Right str <- recvAll socket 6 -- receive 6 characters representing the length of message to read
    | Left err => pure $ Left err
  log $ "received  " ++ str
  case parseInteger (unpack str) 0 of
    Nothing => pure $ Left ("fail to parse '" ++ str ++ "' to expected number of characters, ignoring")
    Just len => do
      Right msg <- recvAll socket (fromInteger len)
        | Left err => pure $ Left (show err)
      pure $ Right msg
