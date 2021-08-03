||| Command-line options when starting up game server
module Bautzen.Options

import Network.Socket.Data

import Data.Strings.Extra

public export
data RunMode : Type where
  ClientMode : RunMode
  ServerMode : RunMode

public export
data Verbosity : Type where

  ||| Do not output anything.
  Quiet : Verbosity

  ||| Output log of every action to standard out, prefixed with given name.
  Verbose : (context : String) -> Verbosity

public export
record Options where
  constructor MkOptions
  ||| The port to listen on for incoming connections
  ||| Can be 0 in which case the server listens on a random port
  port : Port

  ||| The host to bind server to
  ||| defaults to `localhost` which will only allow local connections
  host : String

  ||| The mode this instance runs in, either client or server
  ||| default mode is `ServerMode`
  runMode : RunMode

  ||| Verbosity of output.
  ||| Mostly useful when running in  server mode.
  verbosity : Verbosity

export
defaultOptions : Options
defaultOptions = MkOptions 34567 "localhost" ServerMode (Verbose "bautzen1945")

doProcessOptions : List String -> Options -> Either String Options
doProcessOptions []                        opts = Right opts
doProcessOptions ("--host" :: arg :: args) opts =
  doProcessOptions args (record { host = arg } opts)
doProcessOptions ("--port" :: arg :: args) opts =
  case parseInteger (unpack arg) 0 of
    Nothing => Left $ "cannot parse " ++ arg ++ " as a port number"
    (Just x) => doProcessOptions args (record { port = fromInteger x } opts)
doProcessOptions ("client" :: args) opts =
  doProcessOptions args (record { runMode = ClientMode } opts)
doProcessOptions ("server" :: args) opts =
  doProcessOptions args (record { runMode = ServerMode } opts)
doProcessOptions ("--quiet" :: args) opts =
  doProcessOptions args (record { verbosity = Quiet } opts)
doProcessOptions ("--verbose" :: arg :: args) opts =
  doProcessOptions args (record { verbosity = Verbose arg } opts)
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
    doProcessOptions [ "--port" , "123" ] Options.defaultOptions = Right (MkOptions 123 "localhost" ServerMode (Verbose "bautzen1945"))
  can_parse_port_option = Refl

  can_parse_host_option :
    doProcessOptions [ "--host" , "foo" ] Options.defaultOptions = Right (MkOptions 34567 "foo" ServerMode (Verbose "bautzen1945"))
  can_parse_host_option = Refl

  can_parse_run_mode_option :
    doProcessOptions [ "client" ] Options.defaultOptions = Right (MkOptions 34567 "localhost" ClientMode (Verbose "bautzen1945"))
  can_parse_run_mode_option = Refl

  can_parse_quiet_mode_option :
    doProcessOptions [ "--quiet" ] Options.defaultOptions = Right (MkOptions 34567 "localhost" ServerMode Quiet)
  can_parse_quiet_mode_option = Refl

  can_parse_verbose_mode_option :
    doProcessOptions [ "--verbose", "name" ] Options.defaultOptions = Right (MkOptions 34567 "localhost" ServerMode (Verbose "name"))
  can_parse_verbose_mode_option = Refl

  can_parse_client_mode_with_host_port :
    doProcessOptions [ "client" ,  "--host" , "foo",  "--port" , "123"  ] Options.defaultOptions
      = Right (MkOptions 123 "foo" ClientMode (Verbose "bautzen1945"))
  can_parse_client_mode_with_host_port= Refl
