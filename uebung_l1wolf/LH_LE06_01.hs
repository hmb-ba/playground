import qualified Data.List as L
import qualified Data.Map as M

numUniques :: (Eq a) => [a] -> Int 
numUniques = length . L.nub 

wordNums :: String -> [(String,Int)]
wordNums = map(\ws -> (head ws, length ws)) . L.group . L.sort . L.words

isIn :: (Eq a) => [a] -> [a] -> Bool
isIn needle haystack = any (L.isPrefixOf needle) $ L.tails haystack


