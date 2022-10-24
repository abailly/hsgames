module Bautzen.Store

import Bautzen.Network
import Bautzen.Games
import Bautzen.Id

import JSON

import Language.JSON

import Data.String.Extra

import System.Concurrency
import System.File

export
interface Store (store : Type) where
  read : HasIO io => store -> io (Either String Games)
  write : HasIO io => store -> (event : GamesEvent) -> io (Either String ())

export
record FileStore where
  constructor MkFileStore
  path : String
  lock : Mutex

export
makeFileStore : String -> IO FileStore
makeFileStore path =
  MkFileStore path <$> makeMutex

initialise : Games -> List String -> Either String Games
initialise games [] = Right games
initialise games (line :: lines) =
      -- lines have newline character at end
      case parse (dropLast 1 line) of
        Just json => case fromJSON json of
                       Left err => Left (show err)
                       Right event =>
                          initialise (Games.apply games (GamesResEvent event)) lines
        Nothing => Left ("Failed to parse line: " ++ line)

export
Store FileStore where
  read (MkFileStore path lock) = do
    mutexAcquire lock
    Right (_, lines) <- readFilePage 0 forever path
          | Left err =>  do
             let msg = ("Error reading file " ++ path ++ ": " ++ show err)
             mutexRelease lock
             pure $ Left msg
    mutexRelease lock
    pure $ initialise initialGames lines

  write (MkFileStore path lock) event = do
    mutexAcquire lock
    Right () <- appendFile path $ (show $ cast {to = JSON} event) ++ "\n"
      | Left err => do mutexRelease lock; pure $ Left $ show err
    Right <$> mutexRelease lock
