module Net.Types(module Player,
                 Command(..), GameId, Connection(..), Connections) where

import           Game        (GameId)
import           Interpreter (Connection (..), Connections)
import           Player

data Command = NewGame Int Int             -- ^Starts a game with given number of human players and robots
             | JoinGame PlayerName GameId  -- ^Player joins an existing game
             deriving (Show, Read)

