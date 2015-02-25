import Control.Monad

main = forever $ do 
	line <- getLine 
	if null line 
		then return () 
		else do 
			putStrLn $ reverseWords line

reverseWords :: String -> String 
reverseWords = unwords . map reverse . words 
