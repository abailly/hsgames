module GameServer.Protocol.Messages

import Network.Socket.Raw

import GameServer.Protocol.Binary

%cg chez libidris_net.so

public export
data Message : Type where
  ListGames : Message
  NewGame :  (numHumans, numRobots : Int ) -> Message
  JoinGame : (playerName : String) -> (gameId : String) -> Message
  Action : (payload : String) -> Message
  Bye : Message

export
Show Message where
  show ListGames = "ListGames"
  show (NewGame numHumans numRobots) = "NewGame " ++ show numHumans ++ " " ++ show numRobots
  show (JoinGame playerName gameId) = "JoinGame " ++ playerName ++ " " ++ gameId
  show (Action payload) = "Action " ++ payload
  show Bye = "Bye"


public export
data Result : Type where
  PlayerRegistered : ( playerKey , gameId : String )  -> Result
  NewGameStarted : (gameId : String ) -> Result
  GameStarts : (gameId : String ) -> Result
  GamesList : (games : List String) -> Result
  InputRequired : ( payload : String ) -> Result
  ErrorMessage : (reason : String ) -> Result

export
Show Result where
  show (PlayerRegistered playerKey gameId) = "PlayerRegistered " ++ playerKey ++ " " ++ gameId
  show (NewGameStarted gameId) = "NewGameStarted " ++ gameId
  show (GameStarts gameId) = "GameStarts " ++ gameId
  show (GamesList games) = "GamesList " ++ show games
  show (InputRequired payload) = "InputRequired " ++ show payload
  show (ErrorMessage reason) = "ErrorMessage " ++ show reason

-- Encoder

export
encodeMessage : (prt:BufPtr) -> (offset:Int) -> (msg : Message) -> IO ()
encodeMessage ptr offset ListGames = sock_poke ptr offset 0
encodeMessage ptr offset (NewGame numHumans numRobots) =
  do sock_poke ptr offset 1
     write32be (cast numHumans) (offset + 1) ptr
     write32be (cast numRobots) (offset + 5) ptr
encodeMessage ptr offset (JoinGame playerName gameId) =
  do sock_poke ptr offset 2
     off <- writeString ptr (cast offset + 1) playerName
     _ <- writeString ptr off gameId
     pure ()
encodeMessage ptr offset (Action payload) =
  do sock_poke ptr offset 3
     writeString ptr (cast offset + 1) payload
     pure ()
encodeMessage ptr offset Bye = sock_poke ptr offset 4

export
prepareMessage : (msg : Message) -> IO (BufPtr, Int)
prepareMessage ListGames =
  do buf <- sock_alloc 9
     write64be 1 buf
     sock_poke buf 8 0
     pure (buf, 9)
prepareMessage (NewGame numHumans numRobots) =
  do buf <- sock_alloc (8 + 1 + 4 + 4)
     write64be 9 buf
     sock_poke buf 8 1
     write32be (cast numHumans) 9 buf
     write32be (cast numRobots) 13 buf
     pure (buf, 17)
prepareMessage (JoinGame playerName gameId) =
  do let len = (8 + 1 + (length playerName + 4) + (length gameId + 4))
     buf <- sock_alloc (cast len)
     write64be (cast len - 8) buf
     sock_poke buf 8 2
     off <- writeString buf 9 playerName
     writeString buf off gameId
     pure (buf, cast len)
prepareMessage (Action payload) =
  do let len = 8 + 1 + (length payload + 4)
     buf <- sock_alloc (cast len)
     write64be (cast len - 8) buf
     sock_poke buf 8 3
     off <- writeString buf 9 payload
     pure (buf, cast len)
prepareMessage Bye =
  do buf <- sock_alloc 9
     write64be 1 buf
     sock_poke buf 8 4
     pure (buf, 9)


-- Decoder

decodeNewGame : (ptr : BufPtr) -> (offset : Integer) -> (len : Integer) -> IO (Either String Message)
decodeNewGame ptr offset len =
  do numHumans <- sock_peek ptr (cast offset)
     numRobots <- sock_peek ptr (cast $ offset + 1)
     pure $ Right $ NewGame numHumans numRobots

decodeJoinGame : (ptr : BufPtr) -> (offset : Integer) -> (len : Integer) -> IO (Either String Message)
decodeJoinGame ptr offset len =
  do (playerName, off) <- readString ptr offset
     (gameId, off') <- readString ptr off
     pure $ Right $ JoinGame playerName gameId

decodeAction : (ptr : BufPtr) -> (offset : Integer) -> (len : Integer) -> IO (Either String Message)
decodeAction ptr offset len =
  do (payload, _) <- readString ptr offset
     pure $ Right $ Action payload

export
decodeMessage : (ptr : BufPtr) -> (len : Integer) -> IO (Either String Message)
decodeMessage ptr len =
  do tag <- sock_peek ptr 0
     case tag of
       0 => pure $ Right ListGames
       1 => decodeNewGame ptr 1 len
       2 => decodeJoinGame ptr 1 len
       3 => decodeAction ptr 1 len
       4 => pure $ Right Bye
       _ => pure $ Left $ "unknown tag " ++ show tag

namespace Test

  export
  test_parseMessage : IO ()
  test_parseMessage = do
    (buf,len) <- prepareMessage (Action "payload")
    Right msg <- decodeMessage buf (cast len)
      | Left err => putStrLn ("error " ++ err)
    putStrLn $ show msg
