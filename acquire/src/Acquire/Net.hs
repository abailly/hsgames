{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
module Acquire.Net (PortNumber, runServer, runPlayer, runNewGame, listGames,
                    consoleIO, InOut(..),
                     module Acquire.Net.Types) where

import           Acquire.Net.Game
import           Acquire.Net.Player
import           Acquire.Net.Server
import           Acquire.Net.Types


