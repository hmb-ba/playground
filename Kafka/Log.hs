module Kafka.Log
where

import Data.ByteString

data Offset = Offset Char Char Char Char Char Char Char Char
data Length = Length Char Char Char Char
data Magic = Magic Char
data Attributes = Attributes Char
data Crc = Crc Char Char Char Char
data A = A Char
data B = B Char Char Char Char
data MessageLength = MessageLength Char Char Char Char
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

data Log = Log [LogEntry]
