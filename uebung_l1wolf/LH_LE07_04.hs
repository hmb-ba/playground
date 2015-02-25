data Vector a = Vector a a a 
	deriving (Show) 

vplus :: (Num a) => Vector a -> Vector a -> Vector a
(Vector i j k) `vplus` (Vector l m n) = Vector (i + l) (j + m) (k + n) 

dotProd :: (Num a) => Vector a -> Vector a -> a
(Vector i j k) `dotProd` (Vector l m n) = i*l + j*m + k*n

vmult :: (Num a) => Vector a -> a -> Vector a
(Vector i j k) `vmult` m = Vector (i*m) (j*m) (k*m)

--Derived Instances 
data Person = Person 	{ firstName :: String 
			, lastName :: String 
			, age :: Int 
			} deriving (Eq, Show, Read, Ord)  

mikeD = Person {firstName = "Michael", lastName = "Diamond", age = 43}
adRock = Person {firstName = "Adam", lastName = "Horovitz", age = 41}
mca = Person {firstName = "Adam", lastName = "Yauch", age = 44}

data Day = Monday | Tuesday | Wedensday | Thursday | Friday | Saturday | Sunday
	deriving (Eq, Ord, Show, Read, Bounded, Enum) 


--Type Synonyms 
type PhoneBook = [(String, String)]
type PhoneNumber = String 
type Name = String 

phoneBook :: PhoneBook
phoneBook =
	[("betty", "555-2938")
	,("bonnie", "452-2928")
	,("patsy", "493-2928")
	,("lucille", "205-2928")
	,("wendy", "939-8282")
	,("penny", "853-2492")
]

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool 
inPhoneBook name pnummer pbook = (name, pnummer) `elem` pbook
