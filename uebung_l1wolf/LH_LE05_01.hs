multThree :: Int -> Int -> Int -> Int 
multThree x y z = x * y * z

compareWithHundred :: Int -> Ordering 
compareWithHundred x = compare 100 x

compareWithHundred2 :: Int -> Ordering 
compareWithHundred2 = compare 100 

divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

applyTwice :: (a -> a) -> a -> a 
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = [] 
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys 

zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

flip' :: (a -> b -> c) -> (b -> a -> c) 
flip' f = g
	where g x y = f y x 
