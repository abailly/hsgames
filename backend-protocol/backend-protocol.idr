module Main

import GameServer.Protocol.Options
import GameServer.Protocol.Tester
import GameServer.Protocol.Mock
import System

%cg chez libidris_net.so

main : IO ()
main = do
  (_ :: args) <- getArgs
  case processOptions args of
    Left err => putStrLn err
    Right options@(MkOptions _ _ TesterMode) => do
      Right () <- tester options
        | Left err => putStrLn err
      pure ()
    Right options@(MkOptions _ _ MockMode) => do
      Right () <- mock options
        | Left err => putStrLn err
      pure ()
