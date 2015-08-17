{-# LANGUAGE ExplicitForAll  #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}
module Game(Game(..), Order(Cancel), currentPlayer, newGame, play, possiblePlay) where

import           Game.Core
import           Game.Play
import           Game.Turn


