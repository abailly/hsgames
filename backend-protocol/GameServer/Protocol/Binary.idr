module GameServer.Protocol.Binary

import Data.List
import Network.Socket.Raw
import System

||| Returns an unsigned `Integer` value as a list of bytes
mkBytes : List Int -> Integer -> List Int
mkBytes acc 0 = acc
mkBytes acc n =
  let m = n `mod` 256
      q = n `div` 256
  in mkBytes (cast m :: acc) q

export
readBytes : (ptr: BufPtr) -> (offset:Integer) -> (len: Int) -> List Int -> IO (List Int)
readBytes _  _ 0 acc = pure acc
readBytes ptr offset n acc =
  do byte <- sock_peek ptr (cast offset + n-1)
     readBytes ptr offset (n-1) (byte :: acc)

export
putBytes : (ptr: BufPtr) -> (offset:Integer) -> (len:Int) -> List Int -> IO ()
putBytes ptr offset len bs = putBytes' len bs
  where
    putBytes' : (len:Int) -> List Int -> IO ()
    putBytes' 0 bs = pure ()
    putBytes' n (b::bs) = do
      sock_poke ptr (cast offset + len - n) b
      putBytes' (n-1) bs

mkInteger : (acc:Integer) -> (x:Int) -> Integer
mkInteger acc x = acc * 256 + cast x

padding : Integer -> Nat -> List Int
padding pad len = replicate (fromInteger $ pad - cast len) (the Int 0)

||| Writes an unsigned `Integer` value as a 64 bits in big endian form
||| into a buffer.
||| It assumes that 1/ the Integer is no larger than 2^63 -1 to fit
||| into 64 bits and 2/ the `ptr` points to a buffer with enough
||| room to fit 8 bytes
export
write64be : Integer -> (ptr: BufPtr) -> IO ()
write64be val ptr =
  do let bytes = mkBytes [] val
     let bytes8 = padding 8 (length bytes) ++ bytes
     putBytes ptr 0 8 bytes8


||| Read a 64 bits big endian value as an unsigned `Integer`
||| Assumes there are at least 8 bytes in the buffer pointed at by
||| `ptr`
export
read64be : (ptr: BufPtr) -> IO Integer
read64be ptr =
  do bytes <- readBytes ptr 0 8 []
     pure $ foldl mkInteger 0 bytes

export
write32be :  Integer -> (offset : Int) -> (ptr: BufPtr) -> IO ()
write32be val offset ptr =
  do let bytes = mkBytes [] val
     let bytes8 = padding 8 (length bytes) ++ bytes
     putBytes ptr (cast offset) 8 bytes8

||| Read a 32 bits big endian value as an unsigned `Integer`
||| Assumes there are at least 8 bytes in the buffer pointed at by
||| `ptr`
export
read32be : (ptr: BufPtr) -> (offset : Integer) -> IO Integer
read32be ptr offset =
  do bytes <- readBytes ptr offset 4 []
     pure $ foldl mkInteger 0 bytes

||| Read a length-encoded string from given buffers
||| Strings are assumed to be encoded as a 32 big endian length and
||| followed by utf8 encoded number of bytes
export
readString : (ptr : BufPtr) -> (offset : Integer) -> IO (String, Integer)
readString ptr offset =
  do len <- read32be ptr offset
     chars <- map chr <$> readBytes ptr (offset+4) (cast len) []
     pure $ (pack chars, offset+4+len)

export
writeString : (ptr : BufPtr) -> (offset : Integer) -> String -> IO Integer
writeString ptr offset str =
  do let chars = unpack str
     let len = length chars
     write32be (cast len) (cast offset) ptr
     putBytes ptr (offset + 4) (cast len) (map ord chars)
     pure $ offset + 4 + cast len

namespace Test

  export
  test_parseLength : IO Integer
  test_parseLength = do
    len_buf <- sock_alloc 8
    write64be 12345678901234567 len_buf
    read64be len_buf

  export
  test_parseLength2 : IO Integer
  test_parseLength2 = do
    len_buf <- sock_alloc 8
    write64be (-12345678901234567) len_buf
    read64be len_buf
