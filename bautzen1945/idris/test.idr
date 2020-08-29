import Text.ParseTest

main : IO ()
main = do
  res <- test
  case res of
    Left e => do { putStrLn "Failed:" ; putStrLn e }
    Right () => putStrLn "Success"
