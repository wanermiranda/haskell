module Main where
import Data.Char
main :: IO ()
main = do contents <- getContents
          run $ map read (words$contents)

run (x:xs) = parse_instances x 1 $ take (x*9*9)  xs

parse_instances 0 _ _ =  return()
parse_instances n n1 m = do putStrLn $ "Instancia " ++ show(n1)
                            printBoolPt $ (extract_lines $ take (9*9) m) && (extract_blocks $ take (9*9) m) && (extract_columns $ take (9*9) m)
                            parse_instances (n-1) (n1+1) $ drop (9*9) m


extract_lines [] = True
extract_lines l = checkRepeat (take 9 l) && (extract_lines $ drop 9 l)

extract_columns [] = True
extract_columns l = foldr (\x seed -> seed && checkRepeat x) True (map (\n -> extract_column n l) [0..8])

extract_column _ [] = []
extract_column  n l  = (take 1 $ drop n l) ++ (extract_column n $ drop 9 l)        


extract_blocks [] = True
extract_blocks l = foldr (\x seed -> seed && checkRepeat x) True (map (\n -> extract_block n l) [0..8])
                                             
extract_block _ [] = []
extract_block  n l          
    | n > 8 = []
    | n > 2 = extract_block (n-3) $ drop (9*3) l
    | n > 6 = extract_block (n-6) $ drop (9*3*2) l
    | otherwise = extract_block' 0 $ drop (n*3) l

extract_block' _ [] = []
extract_block' count l 
    | count > 2 = []
    | otherwise = (take 3 l) ++ (extract_block' (count+1) $ drop 9 l)       

checkRepeat l = fst(foldr (\x seed -> (fst(seed) && not (x `elem` snd(seed)), x:snd(seed)) ) (True, []) l )

printBoolPt val
    | val = putStrLn "SIM"
    | otherwise = putStrLn "NAO"
