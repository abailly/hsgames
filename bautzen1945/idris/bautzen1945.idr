module Main

import Bautzen.Options
import Bautzen.REPL

main : IO ()
main = do
  (_ :: args) <- getArgs
  case processOptions args of
    Left err => putStrLn err
    Right options => repl options
