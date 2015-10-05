{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Chinese.Interpreter(newGame,Connection(..),playerInputHandler
                          ,interpretCommand, readDictionary) where

import           Chinese.Dictionary
import           Chinese.Game
import           Chinese.Message
import           Chinese.Player
import           Chinese.Pretty
import           Control.Exception    hiding (Handler)
import           Control.Monad
import           Control.Monad.Prompt
import           Control.Monad.Reader
import qualified Data.Map             as M
import           System.Exit
import           System.IO
import           System.IO.Error

data PlayerInput a where
  GetAnswer :: Game -> PlayerInput Answer
  Quit      :: Game -> PlayerInput ()

type Handler m a = PlayerInput a -> m a

playerInputHandler :: Handler (ReaderT Connections IO) a
playerInputHandler (GetAnswer g@Game{..}) = do
  Cnx hin hout <- (M.! (playerName playerState)) <$> ask
  liftIO $ getAnswer g hin hout
playerInputHandler (Quit _) = do
  broadcast (\ _ (Cnx _ hout) -> (liftIO $ (hPutStrLn hout $ show GameEnds) >> hFlush hout))
  liftIO exitSuccess

broadcast :: (Monad m) => (PlayerName -> Connection -> m ()) -> ReaderT Connections m ()
broadcast f = ask >>= mapM_ (lift . uncurry f) . M.assocs

getAnswer :: Game -> Handle -> Handle -> IO Answer
getAnswer game hin hout = do let q = nextQuestion game
                             hPutStrLn hout $ render $ pretty $ GameState game q
                             hFlush hout
                             r <- tryJust (guard . isEOFError) $ hGetLine hin
                             case r of
                              Left  _    -> return Cancel
                              Right line -> return (interpretAnswer q line)

interpretAnswer :: Question -> String -> Answer
interpretAnswer (CharToPinyin _) reply = Pinyin reply


interpretCommand :: Game -> Prompt PlayerInput Game
interpretCommand game@Game{..} = do
  answer <- prompt $ GetAnswer game
  if answer == Cancel
    then prompt (Quit game) >> return game
    else interpretCommand $ checkAnswer game answer

