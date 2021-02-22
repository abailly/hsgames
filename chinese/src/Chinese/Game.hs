{-# LANGUAGE ExplicitForAll  #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}

module Chinese.Game where

import           Chinese.Dictionary
import           Chinese.Player
import           Data.Time
import           Prelude            hiding (Word)
import           System.IO.Unsafe
import           System.Random

data Question = CharToPinyin Word
                -- ^Ask the pronunciation of a chinese character
              | FrenchToChinese String
                -- ^Ask chinese word for french
              deriving Show

data Answer = Pinyin String
            | Chinese String
            | French String
            | Selected GameType
            | Skip
            | Cancel
            deriving (Show, Read, Eq)

data Result = Correct
              -- ^Provided answer is correct
            | Wrong [Answer]
              -- ^Answer is wrong, and correct answer is provided
              deriving (Show, Read)

data Game = Game { gameDictionary :: Dictionary
                   -- ^Dictionary to use for game, linking chinese characters, pinyin and french
                 , wordsList      :: [ Word ]
                   -- ^random list of wordsList from dictionary, to be used for asking questions according
                   -- to type of question
                 , playerState    :: Player
                 , gameType       :: GameType
                 , gameStartedAt  :: UTCTime
                 , gameEndedAt    :: Maybe UTCTime
                 }

-- | Defines the type of questions that are asked
data GameType = Sound -- ^ Get pinyin for given word
              | Version -- ^ Translate from chinese to french
              | Theme   -- ^ Translate from french to chinese
              deriving (Show,Read,Eq,Enum)

instance Show Game where
  show Game{..} = "Game " ++ show (length gameDictionary) ++
                  " words, type: "  ++ show gameType ++
                  ", player: " ++ show playerState ++
                  ", started: " ++ show gameStartedAt ++
                  ", ended: " ++ show gameEndedAt


newGame :: StdGen -> PlayerName -> Dictionary -> UTCTime -> Game
newGame rand name dict start = let qs g = let (re, g')  = randomWord g dict
                                          in  re : qs g'
                               in Game dict (qs rand) (newPlayer name) Sound start Nothing

{-# NOINLINE randomElement #-}
randomElement :: [ a ] -> a
randomElement as = let g = unsafePerformIO $ randomRIO (0,length as -1)
                   in as !! g

nextQuestion :: Game -> Question
nextQuestion Game{..} | gameType == Sound = CharToPinyin $ head wordsList
                      | gameType == Theme = let (Word _ _ frs) = head wordsList
                                            in FrenchToChinese $ randomElement frs
                      | otherwise        = undefined

checkAnswer :: Game -> Answer -> Result
checkAnswer (Game _ qs _ Sound _ _) (Pinyin s) = let (Word _ p fr) = head qs
                                                 in  if p == s
                                                     then Correct
                                                     else Wrong $ Pinyin p:(map French fr)
checkAnswer (Game _ qs _ Theme _ _) (Chinese s) = let (Word s' p _) = head qs
                                                  in  if s' == s
                                                      then Correct
                                                      else Wrong [Chinese s', Pinyin p]
checkAnswer Game{} Cancel     = Correct
checkAnswer (Game _ qs _ Sound _ _)  _          = let (Word _ p _) = head qs
                                                in Wrong [Pinyin p]
checkAnswer (Game _ qs _ Theme _ _)  _          = let (Word zh p _) = head qs
                                                in Wrong [Chinese zh, Pinyin p]
checkAnswer Game{} _     = undefined -- TODO

correctAnswer  :: Game -> Game
correctAnswer g@Game{..} = g { playerState = successAnswer playerState
                             , wordsList = tail wordsList
                             }

wrongAnswer  :: Game -> Game
wrongAnswer g@Game{..} = g { playerState = failureAnswer playerState
                           , wordsList = tail wordsList
                           }
