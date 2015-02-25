maximum' :: (Ord a) => [a] -> a 
maximum' [] = error "maximum of a empty list"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

replicate' :: Int -> a -> [a] 
replicate' n x
     |  n <= 0  = []
     | otherwise = x : replicate' (n-1) x
	 
take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
     | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x : take'(n-1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' _[] = False 
elem' value (x:xs)
     | x == value = True
     | otherwise = elem' value xs	 