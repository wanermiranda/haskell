module Main where
import Data.Char
main :: IO ()
main = do contents <- getContents
          loop_musi $ map read (words$contents)


loop_musi [] = putStrLn $ show 0
loop_musi (x:xs) 
    | x == 0 = return ()
    | drop x xs == [] = do  putStrLn $ show (compute $ (take x xs))
    | otherwise       = do  putStrLn $ show (compute $ (take x xs))
                            loop_musi $ drop x xs

compute l = compute1 (head l) l (last l)

compute1 h (a:b:tl) le
    -- First Compute - only 2 elements
    | (a == h) && (tl == []) = peak le a b + peak a b h
        -- Last Compute
    | tl == [] = peak a b h
    -- First Compute - and continue
    | (a == h) = peak le a b + compute2 tl
    -- compute with 3 elements - and continue
    | otherwise = compute2 tl
        where compute2 (c:tl2) = peak a b c  + compute1 h (b:c:tl2) le
  


peak a b c 
    | ((a > b) && (b < c)) || ((a < b) && (b > c)) = 1 
    | otherwise = 0