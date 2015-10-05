{-# LANGUAGE ExplicitForAll  #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}

module Chinese.Game where

import           Chinese.Dictionary
import           Chinese.Player
import           Prelude            hiding (Word)
import           System.Random

data Question = CharToPinyin Word
                -- ^Ask the pronunciation of a chinese character
              deriving Show

data Answer = Pinyin String
            | Skip
            | Cancel
            deriving (Show, Read, Eq)

data Game = Game { gameDictionary :: Dictionary
                   -- ^Dictionary to use for game, linking chinese characters, pinyin and french
                 , questions      :: [ Question ]
                   -- ^Pre-computed list of questions for this game. Should be computed lazily using
                   -- random shufflying of dictionary
                 , playerState    :: Player
                 }

instance Show Game where
  show Game{..} = "Game " ++ show (length gameDictionary) ++ " words, next question: " ++ show (head questions)

newGame :: StdGen -> PlayerName -> Dictionary -> Game
newGame rand name dict = let qs g = let (re, g')  = randomWord g dict
                                    in  CharToPinyin re : qs g'
                         in Game dict (qs rand) (newPlayer name)

nextQuestion :: Game -> Question
nextQuestion Game{..} = head questions

checkAnswer :: Game -> Answer -> Game
checkAnswer g@Game{..} (Pinyin s) = let (CharToPinyin (Word _ p _)) = head questions
                                    in  if p == s
                                        then correctAnswer g
                                        else wrongAnswer g
checkAnswer g@Game{..} Skip       = wrongAnswer g
checkAnswer g@Game{..} Cancel     = g

correctAnswer  :: Game -> Game
correctAnswer g@Game{..} = g { playerState = successAnswer playerState
                             , questions = tail questions
                             }

wrongAnswer  :: Game -> Game
wrongAnswer g@Game{..} = g { playerState = failureAnswer playerState
                           , questions = tail questions
                           }



