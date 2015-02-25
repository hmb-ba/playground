map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
	| p x == True = x : filter p xs
	| otherwise = filter p xs  

largestDivisible :: Integer 
largestDivisible = head (filter p [100000,99999..])
	where p x = mod x 3829 == 0

chain :: Integer -> [Integer]
chain 1 = [1]
chain x 
	| even x = x : chain (div x  2)
	| otherwise = x : chain(x*3 + 1)

numLongChains :: Int
numLongChains = length (filter isLong (map chain [1..100]))
	where isLong xs = length xs > 15

numLongChainsL :: Int 
numLongChainsL = length ( filter (\xs -> length xs > 15) (map chain[1..100]))

addThree :: Int -> Int -> Int -> Int 
addThree x y z = x + y + z 
addThree' = \x -> \y -> \z -> x + y + z

flip' :: (a -> b -> c) -> b -> a -> c
flip' f = \x y -> f y x
