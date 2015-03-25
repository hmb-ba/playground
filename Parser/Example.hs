import Data.ByteString
import Control.Monad.State
import Data.Bits
import Control.Applicative
import qualified Data.ByteString.Lazy.Char8 as B
import System.IO
import Data.ByteString.Char8

type Parser = State ByteString

main :: IO ()
main = do
  System.IO.putStrLn $ show $ run interval $ Data.ByteString.Char8.pack $ "\a"


run :: Parser a -> ByteString -> a
run =
  evalState

bsOfSize :: Int -> Parser ByteString
bsOfSize = 
  state . Data.ByteString.splitAt

intOfSize :: (Bits a, Num a) => Int -> Parser a
intOfSize = 
  fmap decodeInt . bsOfSize
  where
    decodeInt = 
      Data.ByteString.foldl' (\n h -> shiftL n 8 .|. fromIntegral h) 0

interval :: Parser Integer
interval =
  udmInterval <$> intOfSize 8 <*> intOfSize 4 <*> intOfSize 4
    where
      udmInterval u d m =
        10 ^ 6 * (u + 10 ^ 6 * 60 * 60 * 24 * (d + 31 * m))
