||| Command-line options when starting up game server
module Bautzen.Options

import Network.Socket.Data
import Bautzen.Id
import Data.Vect
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

  ||| Unique identifier for this instance.
  instanceId : Id

export
defaultOptions : Options
defaultOptions = MkOptions 34567 "localhost" ServerMode (Verbose "bautzen1945") defaultId

doProcessOptions : List String -> Options -> Either String Options
doProcessOptions []                        opts = Right opts
doProcessOptions ("--host" :: arg :: args) opts =
  doProcessOptions args ({ host := arg } opts)
doProcessOptions ("--port" :: arg :: args) opts =
  case parseInteger (unpack arg) 0 of
    Nothing => Left $ "cannot parse " ++ arg ++ " as a port number"
    (Just x) => doProcessOptions args ({ port := fromInteger x } opts)
doProcessOptions ("client" :: args) opts =
  doProcessOptions args ({ runMode := ClientMode } opts)
doProcessOptions ("server" :: args) opts =
  doProcessOptions args ({ runMode := ServerMode } opts)
doProcessOptions ("--quiet" :: args) opts =
  doProcessOptions args ({ verbosity := Quiet } opts)
doProcessOptions ("--verbose" :: arg :: args) opts =
  doProcessOptions args ({ verbosity := Verbose arg } opts)
doProcessOptions ("--id" :: arg :: args) opts =
  makeId arg >>= \ iid => doProcessOptions args ({ instanceId := iid } opts)
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
    doProcessOptions [ "--port" , "123" ] Options.defaultOptions = Right (MkOptions 123 "localhost" ServerMode (Verbose "bautzen1945") (replicate 8 '0'))
  can_parse_port_option = Refl

  can_parse_host_option :
    doProcessOptions [ "--host" , "foo" ] Options.defaultOptions = Right (MkOptions 34567 "foo" ServerMode (Verbose "bautzen1945") (replicate 8 '0'))
  can_parse_host_option = Refl

  can_parse_run_mode_option :
    doProcessOptions [ "client" ] Options.defaultOptions = Right (MkOptions 34567 "localhost" ClientMode (Verbose "bautzen1945") (replicate 8 '0'))
  can_parse_run_mode_option = Refl

  can_parse_quiet_mode_option :
    doProcessOptions [ "--quiet" ] Options.defaultOptions = Right (MkOptions 34567 "localhost" ServerMode Quiet (replicate 8 '0'))
  can_parse_quiet_mode_option = Refl

  can_parse_verbose_mode_option :
    doProcessOptions [ "--verbose", "name" ] Options.defaultOptions = Right (MkOptions 34567 "localhost" ServerMode (Verbose "name") (replicate 8 '0'))
  can_parse_verbose_mode_option = Refl

  can_parse_instance_id_option :
    doProcessOptions [ "--id", "01234567" ] Options.defaultOptions = (makeId "01234567" >>= \ iid => Right (MkOptions 34567 "localhost" ServerMode (Verbose "bautzen1945") iid))
  can_parse_instance_id_option = Refl

  can_parse_client_mode_with_host_port :
    doProcessOptions [ "client" ,  "--host" , "foo",  "--port" , "123"  ] Options.defaultOptions
      = Right (MkOptions 123 "foo" ClientMode (Verbose "bautzen1945") (replicate 8 '0'))
  can_parse_client_mode_with_host_port= Refl
