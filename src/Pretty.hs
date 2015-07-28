{-# LANGUAGE RecordWildCards #-}
module Pretty(Pretty(..), putDoc) where

import           Game
import           Text.PrettyPrint.ANSI.Leijen

instance Pretty HotelChain where
  pretty American    = green $ text "Am"
  pretty Continental = blue $ text "Co"
  pretty Festival    = red $ text "Fe"
  pretty Imperial    = magenta $ text "Im"
  pretty Luxor       = dullgreen $ text "Lu"
  pretty Tower       = dullblue $ text "To"
  pretty Worldwide   = dullcyan $ text "Wo"

instance Pretty Cell where
  pretty (Cell (x,y) Empty)     = char x <> char '-' <> int y
  pretty (Cell (x,y) Neutral)   = char x <> char '-' <> int y
  pretty (Cell (x,y) (Chain h)) = pretty h

instance Pretty Game where
  pretty Game{..} = vcat [ format row | row <- gameBoard ]
    where
      format row = hsep $ map pretty row

