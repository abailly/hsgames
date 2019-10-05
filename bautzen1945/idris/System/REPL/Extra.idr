module System.REPL.Extra

import System.File

||| Process input from an open file handle, while maintaining a state.
||| @ state the input state
||| @ onRead the function to run on reading a line, returning a String to
||| output and a new state
||| @ onEOF the function to run on reaching end of file, returning a String
||| to output
partial
export
processHandle : File ->
                (state : a) ->
                (onRead : a -> String -> (String, a)) ->
                (onEOF : a -> String) ->
                IO ()
processHandle h acc onRead onEOF
   = fEOF h >>= processHandle'
   where
     processHandle' : Bool -> IO ()
     processHandle' iseof =
       if iseof
       then putStr (onEOF acc)
       else do Right x <- fGetLine h
                   | Left err => putStr (onEOF acc)
               let (out, acc') = onRead acc x
               putStr out
               processHandle h acc' onRead onEOF


||| Process input from the standard input stream, while maintaining a state.
||| @ state the input state
||| @ onRead the function to run on reading a line, returning a String to
||| output and a new state
||| @ onEOI the function to run on reaching end of input, returning a String
||| to output
partial
export
processStdin : (state : a) ->
               (onRead : a -> String -> (String, a)) ->
               (onEOI : a -> String) -> IO ()
processStdin = processHandle stdin
