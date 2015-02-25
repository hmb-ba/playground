import Data.Char 

main = do 
	putStrLn "hello, whats your first name?"
	firstName <- getLine 
	putStrLn "What's your last name?"
	lastName <- getLine 
	let bigFirstName = map toUpper firstName 
	let bigLastName = map toUpper lastName
	putStrLn $ "Hey " ++ bigLastName ++ " " ++ bigFirstName ++ " how are you" 
	putStrLn ("Hey " ++ firstName ++ ", you rock!"); 

