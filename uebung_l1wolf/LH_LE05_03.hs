sum' :: (Num b) => [b] -> b
sum' xs =  foldl (\acc x -> acc + x) 0 xs

sum'' :: (Num a) => [a] -> a
sum'' = foldl (+) 0 

map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs

elem' :: (Eq a) => a -> [a] -> Bool
elem' x xs = foldr (\y acc -> if y == x then True else acc) False xs

maximum' :: (Ord a) => [a] -> a 
maximum' = foldl1 max
 
reverse' :: [a] -> [a]
reverse' = foldl (\acc y -> y : acc) []

reverse'' :: [a] -> [a]
reverse'' = foldl (flip (:)) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' f xs = foldr (\y acc -> if f y then y:acc else acc) [] xs

and' :: [Bool] -> Bool
and' = foldr (&&) True 
