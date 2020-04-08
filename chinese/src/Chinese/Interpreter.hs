{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Chinese.Interpreter(newGame,Connection(..),playerInputHandler
                          ,runGame, readDictionary) where

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
import           Data.Time
import           System.Exit
import           System.IO
import           System.IO.Error

data PlayerInput a where
  CorrectAnswer :: Result -> Game -> PlayerInput Game
  SelectGame    :: Game -> PlayerInput Answer
  GetAnswer     :: Game -> PlayerInput Answer
  Quit          :: Game -> PlayerInput ()

type Handler m a = PlayerInput a -> m a

playerInputHandler :: Handler (ReaderT Connections IO) a
playerInputHandler (SelectGame Game{..}) = do
  Cnx hin hout <- (M.! (playerName playerState)) <$> ask
  liftIO $ selectGame hin hout
playerInputHandler (GetAnswer g@Game{..}) = do
  Cnx hin hout <- (M.! (playerName playerState)) <$> ask
  liftIO $ getAnswer g hin hout
playerInputHandler (CorrectAnswer w  g@Game{..}) = do
  Cnx _ hout <- (M.! (playerName playerState)) <$> ask
  liftIO $ hPutStrLn hout $ render $ pretty w
  return $ case w of
            Correct -> correctAnswer g
            _       -> wrongAnswer g
playerInputHandler (Quit g) = do
  broadcast (\ _ (Cnx _ hout) -> (liftIO $ (hPutStrLn hout $ render $ pretty $ GameEnds g) >> hFlush hout))
  liftIO $ withFile ".chinese.stats" AppendMode $ saveGame g
  liftIO exitSuccess

saveGame :: Game -> Handle -> IO ()
saveGame g h = do
  dt <- getCurrentTime
  hPutStrLn h (show $ g { gameEndedAt = Just dt }) >> hFlush h

broadcast :: (Monad m) => (PlayerName -> Connection -> m ()) -> ReaderT Connections m ()
broadcast f = ask >>= mapM_ (lift . uncurry f) . M.assocs

readOrCancel :: Handle -> (String -> Answer) -> IO Answer
readOrCancel hin f = do
  r <- tryJust (guard . isEOFError) $ hGetLine hin
  return $ either (const Cancel) (\ line -> if line == "\\q"
                                            then Cancel
                                            else f line) r

selectGame :: Handle -> Handle -> IO Answer
selectGame hin hout = do
  hPutStrLn hout "Which type of game do you want to play?"
  mapM_ (\ (i,t) -> hPutStrLn hout $ " " ++ show i ++ "- " ++ show t) (zip ([1 .. ] :: [Int]) [Sound, Version,Theme])
  readOrCancel hin (Selected . toEnum . pred . read)

getAnswer :: Game -> Handle -> Handle -> IO Answer
getAnswer game hin hout = do let q = nextQuestion game
                             hPutStrLn hout $ render $ pretty $ GameState game q
                             hFlush hout
                             readOrCancel hin (interpretAnswer q)

interpretAnswer :: Question -> String -> Answer
interpretAnswer (CharToPinyin _)    reply = Pinyin reply
interpretAnswer (FrenchToChinese _) reply = Chinese reply

runGame :: Game -> Prompt PlayerInput Game
runGame game = do
  inp <- prompt $ SelectGame game
  case inp of
    Selected typ -> interpretCommand (game { gameType = typ })
    _            -> pure game

interpretCommand :: Game -> Prompt PlayerInput Game
interpretCommand game@Game{..} = do
  answer <- prompt $ GetAnswer game
  if answer == Cancel
    then prompt (Quit game) >> return game
    else prompt (CorrectAnswer (checkAnswer game answer) game) >>= interpretCommand
