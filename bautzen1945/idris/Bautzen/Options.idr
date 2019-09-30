||| Command-line options when starting up game server
module Bautzen.Options

import Network.Socket.Data

%default total

export
record Options where
  constructor MkOptions
  ||| The port to listen on for incoming connections
  ||| Can be 0 in which case the server listens on a random port
  port : Port

export
defaultOptions : Options
defaultOptions = MkOptions 34567

parseInteger : List Char -> Integer -> Maybe Integer
parseInteger []        acc = Just acc
parseInteger (c :: cs) acc =
  if (c >= '0' && c <= '9')
  then parseInteger cs ((acc * 10) + (cast ((ord c) - (ord '0'))))
  else Nothing

doProcessOptions : List String -> Options -> Either String Options
doProcessOptions []                        opts = Right opts
doProcessOptions ("--port" :: arg :: args) opts =
  case parseInteger (unpack arg) 0 of
    Nothing => Left $ "cannot parse " ++ arg ++ " as a port number"
    (Just x) => doProcessOptions args (record { port = fromInteger x } opts)
doProcessOptions (arg :: _) _ = Left $ "unknown argument " ++ arg

||| Process arguments from the command-line, turning them into
||| `Options`.
|||
||| Returns a `Left` if there's an error in the processing, or
||| a `Right` with the `Options` data filled in.
export
processOptions : List String -> Either String Options
processOptions args = doProcessOptions args defaultOptions


namespace OptionsTest
  %access private

  can_parse_port_option :
    doProcessOptions [ "--port" , "123" ] Options.defaultOptions = Right (MkOptions 123)
  can_parse_port_option = Refl
