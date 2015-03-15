--module Kafka.Log
--where

import Prelude hiding (take)
import Data.ByteString.Char8 hiding (take)
import Data.Attoparsec.ByteString
import Control.Applicative
import System.IO

--data Offset = Offset Char Char Char Char Char Char Char Char
--data Offset = Offset Int64
data Offset = Offset ByteString deriving (Show, Read)
offsetValue :: Offset -> ByteString --just for debug purposes
offsetValue (Offset offsetValue) = offsetValue

--data Length = Length Char Char Char Char
--data Length = Length Int32
data Length = Length ByteString

data Magic = Magic Bool
data Attributes = Attributes Bool

--data Crc = Crc Char Char Char Char
data Crc = Crc ByteString

--data A = A Char
data A = A ByteString

--data B = B Char Char Char Char
data B = B ByteString

--data MessageLength = MessageLength Char Char Char Char
data MessageLength = MessageLength Int

data Message = Message ByteString
data Payload = Payload A B MessageLength Message
data LogEntry = LogEntryDefault { offset  :: Offset
                                , length  :: Length
                                , magic   :: Magic
                                , crc     :: Crc
                                , payload :: Payload
                                }
              | LogEntryAnnotated { offset     :: Offset
                                  , length     :: Length
                                  , magic      :: Magic
                                  , attributes :: Attributes
                                  , crc        :: Crc
                                  , payload    :: Payload
                                  }

type Log =  [LogEntry]

main :: IO ()
main = do
  file <- Data.ByteString.Char8.readFile "00000000000000000000.log"
  let d = parseOnly offsetParser file
  case d of
    Left l -> System.IO.putStrLn $ show "error"
    Right x -> do
      let s = show $ offsetValue x
      System.IO.putStrLn s

bParser :: Parser B
bParser = do
  b <- take 4
  return $ B b

aParser :: Parser A
aParser = do
  a <- take 1
  return $ A a

crcParser :: Parser Crc
crcParser = do
  c <- take 4
  return $ Crc c

attributesParser :: Parser Attributes
attributesParser = do
  a <- (string (pack "1") >> return True) <|> return False
  return $ Attributes a

magicParser :: Parser Magic
magicParser = do
  m <- (string (pack "1") >> return True) <|> return False
  return $ Magic m

lengthParser :: Parser Length
lengthParser = do
  l <- take 4
  return $ Length l

offsetParser :: Parser Offset
offsetParser = do
  o <- take 8
  return $ Offset o


