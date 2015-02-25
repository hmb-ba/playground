pointStyle1 x = ceiling (negate (tan (cos (max 50 x))))
pointStyle2 = ceiling . negate . tan . cos . max 50 

oddSquareSum :: Integer 
oddSquareSum = sum . takeWhile (<10000) . filter odd $ map (^2) [1..]


