import qualified Data.Map as Map 
import qualified Data.Char as C 

phoneBook :: Map.Map String String 
phoneBook = Map.fromList $
	[("betty", "555-2938")
	,("bonnie", "452-2928")
	,("patsy", "439-2928")
	,("lucille", "205-2928")
	,("wendy", "939-8282")
	,("penny", "853-2492")
	]

string2Digits :: String -> [Int]
string2Digits = map C.digitToInt . filter(\c -> C.isDigit c) 
