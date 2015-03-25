import Data.Word
import Data.Binary.Get
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS

data Payload = Payload
  { keylen      :: !Word32
  --todo: keylen
  , payloadLen  :: !Word32
  , payloadData :: BS.ByteString
  } deriving (Show)

data LogEntry = LogEntry
  { offset  :: !Word64
  , len     :: !Word32
  , crc     :: !Word32
  , magic   :: !Word8
  , attr    :: !Word8
  , payload :: Payload
  } deriving (Show)

entryParser :: Get LogEntry
entryParser = do
  offset <- getWord64be
  len    <- getWord32be
  crc    <- getWord32be
  magic  <- getWord8
  attr   <- getWord8
  p      <- payloadParser
  return $! LogEntry offset len crc magic attr p

payloadParser :: Get Payload
payloadParser = do
  keylen <- getWord32be
  paylen <- getWord32be
  payload <- getByteString $ fromIntegral paylen
  return $! Payload keylen paylen payload

getEntries :: Get [LogEntry]
getEntries = do
  empty <- isEmpty
  if empty
      then return []
      else do entry <- entryParser
              entries <- getEntries
              return (entry:entries)

lazyIOExample :: IO [LogEntry]
lazyIOExample = do
  input <- BL.readFile "logfiles/00000000000000000000.log"
  let t = runGet getEntries input
  print t
  return t

main :: IO ()
main = do
  lazyIOExample
  putStrLn "done"

