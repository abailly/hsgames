||| Command-line options when starting up game server
module Bautzen.Options

import Network.Socket.Data

import Data.String.Extra



export
record Options where
  constructor MkOptions
  ||| The port to listen on for incoming connections
  ||| Can be 0 in which case the server listens on a random port
  port : Port

  ||| The host to bind server to
  ||| defaults to `localhost` which will only allow local connections
  host : String

export
defaultOptions : Options
defaultOptions = MkOptions 34567 "localhost"

doProcessOptions : List String -> Options -> Either String Options
doProcessOptions []                        opts = Right opts
doProcessOptions ("--host" :: arg :: args) opts =
  doProcessOptions args (record { host = arg } opts)
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


  can_parse_port_option :
    doProcessOptions [ "--port" , "123" ] Options.defaultOptions = Right (MkOptions 123 "localhost")
  can_parse_port_option = Refl

  can_parse_host_option :
    doProcessOptions [ "--host" , "foo" ] Options.defaultOptions = Right (MkOptions 34567 "foo")
  can_parse_host_option = Refl
