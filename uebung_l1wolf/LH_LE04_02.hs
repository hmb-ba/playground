quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
     let smallerOrEqual = [ a | a <- xs, a <= x ]
         greater = [ a | a <- xs, a > x ]
	 in  quicksort smallerOrEqual ++ [x] ++ quicksort greater
	 
	 