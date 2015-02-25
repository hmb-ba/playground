findKey :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey key [] = Nothing 
findKey key ((k,v):xs)
	| key == x = Just v
	| otherwise = findKey key xs

findKey' :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey' key xs = foldr (\(k,v) acc -> if key == k then Just v else acc) Nothing x
