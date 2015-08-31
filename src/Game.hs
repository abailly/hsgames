{-# LANGUAGE ExplicitForAll  #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- = How to Play Acquire
--
-- These game rules are stolen from <http://www.cs.cmu.edu/~lanthony/classes/SEng/Design/acquire.html this page> which
-- is part of CS curriculum.
--
-- The main object of Acquire is to become the wealthiest player by the end of the game.  This is done by forming
-- hotel chains, shrewdly buying the right stock at the right time, merging chains to obtain capital and adding
-- hotels to the chains in which you have controlling interest to increase their value.
module Game(Game(..), GameBoard, Order(Cancel), currentPlayer, newGame, play, possiblePlay) where

import           Cells
import           Game.Core
import           Game.Play
import           Game.Turn


