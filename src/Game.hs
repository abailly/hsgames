{-# LANGUAGE ExplicitForAll  #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}
module Game(Game(..), GameBoard, Order(Cancel), currentPlayer, newGame, play, possiblePlay) where

import           Cells
import           Game.Core
import           Game.Play
import           Game.Turn


