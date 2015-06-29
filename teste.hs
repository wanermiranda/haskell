module Main where
import Data.Char
main :: IO ()
main = do contents <- getContents
          mapM_ encotel $ lines contents

encotel :: String -> IO()
encotel str = putStrLn (foldr (\c seed -> getNum c :seed) "" str)
    where getNum chr 
            | toUpper(chr) `elem` "ABC" = '2'        
            | toUpper(chr) `elem` "DEF" = '3'
            | toUpper(chr) `elem` "GHI" = '4'
            | toUpper(chr) `elem` "JKL" = '5'
            | toUpper(chr) `elem` "MNO" = '6'
            | toUpper(chr) `elem` "PQRS" = '7'
            | toUpper(chr) `elem` "TUV" = '8'
            | toUpper(chr) `elem` "WXYZ" = '9'
            | otherwise = chr
