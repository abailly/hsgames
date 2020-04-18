module GameServer.Protocol.Options

import Network.Socket.Data

public export
data RunMode : Type where
  TesterMode : RunMode
  MockMode : RunMode

public export
record Options where
  constructor MkOptions
  ||| The port to use for protocol, either to listen or connect to
  port : Port

  ||| The host to bind server to or connect to
  ||| defaults to `localhost` which will only allow local connections
  host : String

  ||| The mode this instance runs in
  ||| default mode is `TesterMode`
  runMode : RunMode

parseInteger : List Char -> Integer -> Maybe Integer
parseInteger []        acc = Just acc
parseInteger (c :: cs) acc =
  if (c >= '0' && c <= '9')
  then parseInteger cs ((acc * 10) + ((cast c) - (cast '0')))
  else Nothing

export
defaultOptions : Options
defaultOptions = MkOptions 34567 "localhost" TesterMode

doProcessOptions : List String -> Options -> Either String Options
doProcessOptions []                        opts = Right opts
doProcessOptions ("--host" :: arg :: args) opts =
  doProcessOptions args (record { host = arg } opts)
doProcessOptions ("--port" :: arg :: args) opts =
  case parseInteger (unpack arg) 0 of
    Nothing => Left $ "cannot parse " ++ arg ++ " as a port number"
    (Just x) => doProcessOptions args (record { port = fromInteger x } opts)
doProcessOptions ("tester" :: args) opts =
  doProcessOptions args (record { runMode = TesterMode } opts)
doProcessOptions ("mock" :: args) opts =
  doProcessOptions args (record { runMode = MockMode } opts)
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
    doProcessOptions [ "--port" , "123" ] Options.defaultOptions = Right (MkOptions 123 "localhost" TesterMode)
  can_parse_port_option = Refl

  can_parse_host_option :
    doProcessOptions [ "--host" , "foo" ] Options.defaultOptions = Right (MkOptions 34567 "foo" TesterMode)
  can_parse_host_option = Refl

  can_parse_run_mode_option :
    doProcessOptions [ "mock" ] Options.defaultOptions = Right (MkOptions 34567 "localhost" MockMode )
  can_parse_run_mode_option = Refl

  can_parse_mock_mode_with_host_port :
    doProcessOptions [ "mock" ,  "--host" , "foo",  "--port" , "123"  ] Options.defaultOptions
      = Right (MkOptions 123 "foo" MockMode )
  can_parse_mock_mode_with_host_port= Refl
