{-# LANGUAGE RecordWildCards #-}
module Pretty(Pretty(..), putDoc) where

import           Data.Array
import qualified Data.Map                     as M
import           Game
import           Text.PrettyPrint.ANSI.Leijen

instance Pretty Tile where
  pretty (Tile (x,y)) = fill 4 $ char x <> char '-' <> int y

instance Pretty Player where
  pretty (Player name tiles stock) = text name <+> list (map pretty tiles) <+>
                                     list (map (\ (n,q) -> pretty n <+> text "->" <+> int q) $ M.toList stock)

instance Pretty ChainName where
  pretty American    = ondullred   $ fill 4 $ green     $ text "Am"
  pretty Continental = ondullred   $ fill 4 $ blue      $ text "Co"
  pretty Festival    = oncyan      $ fill 4 $ red       $ text "Fe"
  pretty Imperial    = oncyan      $ fill 4 $ magenta   $ text "Im"
  pretty Luxor       = oncyan      $ fill 4 $ dullgreen $ text "Lu"
  pretty Tower       = oncyan      $ fill 4 $ dullblue  $ text "To"
  pretty Worldwide   = ondullgreen $ fill 4 $ dullcyan  $ text "Wo"

instance Pretty Cell where
  pretty (Cell t Empty)                = pretty t
  pretty (Cell t (Neutral _))          = dullred $ pretty t
  pretty (Cell (Tile (x,y)) (Chain h)) = pretty h

instance Pretty Game where
  pretty Game{..} = (vcat $ rows gameBoard) <$$>
                    (vcat $ map pretty (M.elems players))
    where
      rows   board = [ hsep $ map (\ y -> pretty $ board ! Tile (x, y)) [ 1 .. 12 ] | x <- ['A' .. 'I'] ]

