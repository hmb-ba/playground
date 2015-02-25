data Car' = Car' { company' :: String
		, model' :: String 
		, year' :: Int 
		} deriving (Show) 

data Car a b c = Car { company :: a 
		, model :: b 
		, year :: c 
		} deriving (Show) 

tellCar :: (Show a) => Car String String a -> String 
tellCar (Car {company = c, model = m, year = y}) = 
	"This " ++ c ++ " " ++ m ++ " was made in " ++ show y

