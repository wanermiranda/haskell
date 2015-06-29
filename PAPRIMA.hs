module Main where
import Data.Char
main :: IO ()
main = do contents <- getContents
          mapM_ print_paprima $ lines contents

--paprima :: String -> IO()
isprime 1 = False
isprime 2 = True
isprime 3 = True
isprime n = ((n `mod` 2) /= 0) && isprime1 n (n `div` 2)
    where isprime1 n d 
            | d <= 3 = True
            | otherwise = ((n `mod` d) /= 0) && (isprime1 n (d-1))

paprima str = isprime $ foldr (\c seed -> getNum c + seed) 0 str
    where getNum chr 
            | isUpper(chr) = (ord chr - 65) + 27
            | otherwise = (ord chr - 97) + 1

print_paprima str 
    | paprima str = putStrLn "It is a prime word."
    | otherwise = putStrLn "It is not a prime word."
