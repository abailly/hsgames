module Chinese.Message where

import           Chinese.Game
import           Chinese.Player
import qualified Data.Map       as M
import           System.IO

data Connection = Cnx { hIn  :: Handle
                      , hOut :: Handle
                      } deriving (Show)

data Message = GameEnds Game
             | GameState Game Question
               deriving (Show)

closeConnection :: Connection -> IO ()
closeConnection (Cnx hin hout) = hClose hin >> hClose hout

type Connections = M.Map PlayerName Connection

