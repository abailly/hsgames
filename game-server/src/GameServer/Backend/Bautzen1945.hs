{-# LANGUAGE NumericUnderscores #-}

-- | Manages an instance of a backend server for /Bautzen 1945/ game.
module GameServer.Backend.Bautzen1945 where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async)
import Control.Exception (Exception, bracket, throwIO)
import Control.Monad (unless, (>=>))
import Data.Aeson (ToJSON (toJSON), Value (Array), encode, object, (.=))
import Data.ByteString (packCStringLen)
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (ByteString, hPut)
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text, unpack)
import Data.Text.Encoding (decodeUtf8)
import Foreign (allocaBytes)
import GHC.Stack (HasCallStack)
import GameServer.Clients.IO (ServerConnection (..), connectSocketTo)
import System.IO (Handle, IOMode (ReadWriteMode), hClose, hFlush, hGetBuf, stdout, withBinaryFile)
import System.Process (CreateProcess (..), StdStream (CreatePipe, UseHandle), proc, withCreateProcess)
import Text.Printf (hPrintf)

withBautzenServer ::
    HasCallStack =>
    (ServerConnection IO -> IO a) ->
    IO a
withBautzenServer action = do
    let process = proc "/Users/arnaud/projects/hsgames/bautzen1945/idris/build/exec/bautzen1945" ["--port", "9999"]
    withBinaryFile "bautzen.log" ReadWriteMode $ \hdl -> do
        withCreateProcess process{std_out = UseHandle hdl} $ \_ _ _ pid -> do
            threadDelay 3_000_000
            cnx <- connectSocketTo "localhost" 9999 >>= makeBautzenConnection
            action cnx

makeBautzenConnection :: Handle -> IO (ServerConnection IO)
makeBautzenConnection hdl =
    let send bs = do
            hPrintf hdl "%06d" $ LBS.length bs
            hPut hdl bs
        receive =
            allocaBytes 6 $ \lenBuf -> do
                rd <- hGetBuf hdl lenBuf 6
                unless (rd == 6) $ error "invalid length read, bailing out"
                lenbs <- packCStringLen (lenBuf, 6)
                let len = read $ unpack $ decodeUtf8 lenbs
                allocaBytes len $ \dataBuf -> do
                    rd <- hGetBuf hdl dataBuf len
                    unless (rd == len) $ error "invalid data length read, bailing out"
                    LBS.fromStrict <$> packCStringLen (dataBuf, len)
        close = hClose hdl
     in pure ServerConnection{..}
