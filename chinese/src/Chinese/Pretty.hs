{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Chinese.Pretty(Pretty(..), module Text.PrettyPrint.ANSI.Leijen, render) where

import           Chinese.Dictionary
import           Chinese.Game
import           Chinese.Message
import           Chinese.Player
import           Data.Time
import           System.IO.Unsafe
import           Text.PrettyPrint.ANSI.Leijen hiding (line, (<$>))

instance Pretty Question where
  pretty (CharToPinyin (Word c _ _)) = text "Pinyin for " <> text c <> text " ?"
  pretty (FrenchToChinese f)         = text "Chinese for " <> text f <> text " ?"

instance Pretty Result where
  pretty Correct   = text "That's correct!"
  pretty (Wrong a) = text "That's wrong, correct answer is: " <> pretty a

instance Pretty Answer where
  pretty (Pinyin p)   = text p
  pretty (Chinese zh) = text zh
  pretty (French fra) = text fra
  pretty a            = text $ show a

instance Pretty Player where
  pretty p@(Player name numQs numErrs ) = text name <+> countErrs <+> int (truncate $ successRate p * 100) <> char '%'
    where
      countErrs = onyellow $ black $ brackets (dullred (int numErrs) <> char '/' <> green (int (numQs - numErrs)))

instance Pretty UTCTime where
  pretty = text . show

instance Pretty NominalDiffTime where
  pretty = text . show

instance Pretty GameType where
  pretty = text . show

instance Pretty Message where
  pretty (GameState Game{..} q) = pretty playerState <$$>
                                  pretty q

  pretty (GameEnds Game{..})    = text "Chinese Training " <+> parens (pretty gameType) <$$>
                                  text "Final score:" <+> pretty playerState  <$$>
                                  pretty timings
    where
      duration = diffUTCTime endTime gameStartedAt
      endTime = unsafePerformIO getCurrentTime
      timings = text "Started game:"  <+> pretty gameStartedAt <$$>
                text "Duration:"      <+> pretty duration <$$>
                text "Sec/question:"  <+> double (realToFrac $ duration / fromIntegral (numberOfAnswers playerState))

render :: Doc -> String
render = flip displayS "" . renderPretty 0.5 132
