module Main

import Bautzen.Options
import Bautzen.REPL
import Bautzen.Server

main : IO ()
main = do
  (_ :: args) <- getArgs
  case processOptions args of
    Left err => putStrLn err
    Right options => do
      Right () <- server options
        | Left err => putStrLn err
      pure ()
