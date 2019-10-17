module Main

import Bautzen.Options
import Bautzen.REPL
import Bautzen.Server
import System

%cg chez libidris_net.so

main : IO ()
main = do
  (_ :: args) <- getArgs
  case processOptions args of
    Left err => putStrLn err
    Right options => do
      Right () <- server options
        | Left err => putStrLn err
      pure ()
