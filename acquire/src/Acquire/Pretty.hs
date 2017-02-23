{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Acquire.Pretty(Pretty(..), module Text.PrettyPrint.ANSI.Leijen, render) where

import           Acquire.Game
import           Data.Array
import qualified Data.Map                     as M
import           Text.PrettyPrint.ANSI.Leijen

instance Pretty Tile where
  pretty (Tile (x,y)) = fill 4 $ char x <> char '-' <> int y

instance Pretty PlayerType where
  pretty Human = char 'H'
  pretty Robot = char 'R'

instance Pretty Player where
  pretty (Player name typ tiles stock cash ) = text name <+> brackets (pretty typ) <+>
                                               align (list (map pretty tiles) <$$>
                                                      list (map (\ (n,q) -> pretty n <+> text "->" <+> int q) $ listStock stock) <+> char '$' <> int cash)

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
  pretty (Cell t Playable)             = onyellow $ dullred $ pretty t
  pretty (Cell t (Neutral _))          = dullred $ pretty t
  pretty (Cell (Tile (_,_)) (Chain h)) = pretty h

instance Pretty GameBoard where
  pretty gameBoard = vcat $ rows gameBoard
    where
      rows   board = [ hsep $ map (\ y -> pretty $ board ! Tile (x, y)) [ 1 .. 12 ] | x <- ['A' .. 'I'] ]

instance Pretty Game where
  pretty Game{..} = pretty gameBoard <$$>
                    (vcat $ map pretty (M.elems players))


render :: Doc -> String
render = flip displayS "" . renderPretty 0.5 132
