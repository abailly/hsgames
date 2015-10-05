{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Chinese.Pretty(Pretty(..), module Text.PrettyPrint.ANSI.Leijen, render) where

import           Chinese.Message
import           Chinese.Game
import           Chinese.Dictionary
import           Chinese.Player
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>), line)


instance Pretty Question where
  pretty (CharToPinyin (Word c _ _)) = text "Pinying for " <> text c <> text " ?"
  
instance Pretty Player where
  pretty p@(Player name numQs numErrs ) = text name <+> countErrs <+> double (successPercent p)
    where
      countErrs = onyellow $ dullred $ brackets (int numErrs <> char '/' <> int numQs)
      
instance Pretty Message where
  pretty (GameState Game{..} q) = pretty playerState <$$>
                                  pretty q

  pretty GameEnds               = text "The End" 


render :: Doc -> String
render = flip displayS "" . renderPretty 0.5 132
