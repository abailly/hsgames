{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
module Net.Server(runServer, PortNumber) where

import           Control.Monad.Prompt
import           Control.Monad.Reader
import qualified Data.Map             as M
import           Interpreter
import           Net.Client
import           Network.Socket
import           Player
import           System.IO
import           System.Random

runServer :: PortNumber -> Int -> Int -> IO ()
runServer port numHumans numRobots = do
  sock <- socket AF_INET Stream defaultProtocol
  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet port iNADDR_ANY)
  listen sock 5
  clients <- waitForClients sock numHumans [] >>= mapM getHandle
  close sock
  g <- getStdGen
  let connections = M.fromList $ ("Console", Cnx stdin stdout) : map (\ (p,h) -> (p, Cnx h h)) clients
  let robots = map ((,Robot) . ("robot " ++) . show) [ 1 .. numRobots ]
  runReaderT (runPromptM playerInputHandler $ initialisedGame g (map (\ (p,_) -> (p,Human)) clients ++ robots) >>= interpretCommand) connections
    where
      getHandle (player,sock) = do
        h <- socketToHandle sock ReadWriteMode
        hSetBuffering h NoBuffering
        return (player,h)
