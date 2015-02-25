import qualified Data.Char as C
import qualified Data.List as L

encode :: Int -> String -> String 
encode key msg = map (\c -> C.chr $ C.ord c + key) msg 

decode :: Int -> String -> String 
decode key = map (\c -> C.chr $ C.ord c - key)

digitSum :: Int -> Int 
digitSum n = sum . map C.digitToInt . show $ n

firstTo :: Int ->  Maybe Int 
firstTo n = L.find (\x -> digitSum x == n) [1..]
