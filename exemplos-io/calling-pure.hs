import Data.Char

-- file: ch07/callingpure.hs
name2reply :: String -> String
name2reply name =
    "Pleased to meet you, " ++ name ++ ".\n" ++
    "Your name contains " ++ charcount ++ " characters."
    where charcount = show (length name)

main :: IO ()
main = do
       putStrLn "Greetings once again.  What is your name?"
       inpStr <- getLine
       let outStr = name2reply inpStr
       putStrLn outStr

numeroLetras::String->String
numeroLetras nome = show(length nome)

setima = do{
	test <-getLine;	
	--putStrLn $ show $take (read test) (lines contents)
	putStrLn $ show (map toUpper test)
}

par n
	| n == 2 = Just n
	| otherwise = Nothing

dois n = do
	m <- par n
	return m

numero = do 
    putStrLn "Insert a string to convert: "
    -- Input string
    line <- getLine
    return (lines line)