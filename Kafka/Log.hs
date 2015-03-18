--module Kafka.Log
--where

import Prelude hiding (take)
import Data.ByteString hiding (take)
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Char8 as Char8 
import Data.Attoparsec.ByteString.Lazy
import Control.Applicative
import System.IO
--import Data.Binary
import Data.Serialize
import Data.Int

--data Offset = Offset Char Char Char Char Char Char Char Char
--data Offset = Offset Int64
type Offset = Int64
type Length = Int32
type Magic = Bool
type Attributes = Bool
type Crc = String
type A = Char
type B = String
type MessageLength = Length
type Message = String

data Payload = Payload A B MessageLength Message deriving (Show)
data LogEntry = LogEntryDefault { offset  :: Offset
                                , len :: Length
                                , magic   :: Magic
                                , crc     :: Crc
                                , payload :: Payload
                                }
              | LogEntryAnnotated { offset     :: Offset
                                  , len     :: Length
                                  , magic      :: Magic
                                  , attributes :: Attributes
                                  , crc        :: Crc
                                  , payload    :: Payload
                                  }
              deriving (Show)

type Log =  [LogEntry]

main :: IO ()
main = do
  file <- BS.readFile "00000000000000000000.log"
  print $ BS.unpack file
  print "hello"
  parseTest entryParser file
  print "nacher"
  --System.IO.putStrLn $ show $ offsetValue d
  
  --file <- Char8.readFile "00000000000000000000.log"
  --let d = parseOnly offsetParser file
  --case d of
  --  Left l -> System.IO.putStrLn $ show "error"
  --  Right x -> do
  --    System.IO.putStrLn $ show x

entryParser :: Parser LogEntry
entryParser = do
  o <- offsetParser
  l <- lengthParser
  m <- magicParser
  --todo: parse attribute field if magic == 1
  c <- crcParser
  p <- payloadParser
  return $ LogEntryDefault { offset = o
                           , len = l
                           , magic = m
                           , crc = c
                           , payload = p
                           }

payloadParser :: Parser Payload
payloadParser = do
  a <- aParser
  b <- bParser
  ml <- lengthParser
  m <- messageParser ml
  return $ Payload a b ml m

messageParser :: Length -> Parser Message
messageParser a = do 
  m <- take $ fromIntegral a
  case decode m of
    Left l -> return "m"
    Right r -> return r

bParser :: Parser B
bParser = do
  b <- take 4
  case decode b of
    Left l -> return "0"
    Right r -> return r

aParser :: Parser A
aParser = do
  a <- take 1
  case decode a of
    Left l -> return '0'
    Right r -> return r

crcParser :: Parser Crc
crcParser = do
  c <- take 4
  case decode c of 
    Left l -> return "0"
    Right r -> return r

attributesParser :: Parser Attributes
attributesParser = do
  a <- (string (Char8.pack "1") >> return True) <|> return False
  return a

magicParser :: Parser Magic
magicParser = do
  m <- (string (Char8.pack "1") >> return True) <|> return False
  return m

lengthParser :: Parser Length
lengthParser = do
  l <- take 4
  case (decode l) of
    Left l -> return $ 0
    Right r -> return $ r

offsetParser :: Parser Offset
offsetParser = do
  o <- take 8
  let offset = decode o 
  case offset of
    Left l -> return 0
    Right r -> return r


