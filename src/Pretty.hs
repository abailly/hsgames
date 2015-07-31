{-# LANGUAGE RecordWildCards #-}
module Pretty(Pretty(..), putDoc) where

import           Data.Array
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
  pretty (Cell (Tile (x,y)) Empty)       = char x <> char '-' <> int y
  pretty (Cell (Tile (x,y)) (Neutral _)) = dullred $ char x <> char '-' <> int y
  pretty (Cell (Tile (x,y)) (Chain h))   = pretty h

instance Pretty Game where
  pretty Game{..} = vcat $ rows gameBoard
    where
      rows   board = [ hsep $ map (\ y -> pretty $ board ! (x, y)) [ 1 .. 12 ] | x <- ['A' .. 'I'] ]

