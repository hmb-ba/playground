--Tree 
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show)

-- Traffic Light 
data TrafficLight = Red | Yellow | Green 

instance Eq TrafficLight where 
	Red == Red = True 
	Green == Green = True 
	Yellow  == Yellow = True 
	_ == _ = False 

instance Show TrafficLight where 
	show Red = "Red light"
	show Green = "Green light"
	show Yellow = "Yellow light"

 --YesNo 
class YesNo a where 
	yesno :: a -> Bool 

instance YesNo Int where 
	yesno 0 = False
	yesno _ = True 

instance YesNo [a] where
	yesno [] = False 
	yesno _ = True

instance YesNo Bool where 
	yesno = id 

instance YesNo (Maybe a) where 
	yesno (Just _) = True
	yesno Nothing = False 

instance YesNo (Tree a) where 
	yesno EmptyTree = False
	yesno _ = True

instance YesNo TrafficLight where 
	yesno Red = False
	yesno _ = True

yesnoIf :: (YesNo y) => y -> a -> a -> a 
yesnoIf yesnoVal yesResult noResult = 
	if yesno yesnoVal
	then yesResult
	else noResult 
