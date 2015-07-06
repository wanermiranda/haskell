module Main where
import Data.Char
main :: IO ()
main = do contents <- getContents
          run $ map read (words$contents)

run (x:xs) = parse_instances x 1 $ take (x*9*9)  xs

parse_instances 0 _ _ =  return()
parse_instances n n1 m = do putStrLn $ "lines " ++ show(n1)
                            extract_lines $ take (9*9) m
                            putStrLn $ "columns " ++ show(n1)
                            extract_blocks $ take (9*9) m
                            extract_columns $ take (9*9) m
                            parse_instances (n-1) (n1+1) $ drop (9*9) m


extract_lines [] = return ()
extract_lines l = do    putStrLn (show (take 9 l))
                        extract_lines $ drop 9 l

extract_columns [] = return ()
extract_columns l = do  putStrLn "Columns"
                        mapM_ (\n -> extract_column n l) [0..8]

--                         

extract_column _ [] = return ()
extract_column  n l          
    | n > 8 = return ()
    | otherwise = putStrLn $ show $ extract_column' n l 

extract_column' _ [] = []
extract_column'  n l  = (take 1 $ drop n l) ++ (extract_column' n $ drop 9 l)        




extract_blocks [] = return ()
extract_blocks l = do  putStrLn "blocks"
                       mapM_ (\n -> extract_block n l) [0..8]
                        
                        

extract_block _ [] = return ()
extract_block  n l          
    | n > 8 = return ()
    | n > 2 = extract_block (n-3) $ drop (9*3) l
    | n > 6 = extract_block (n-6) $ drop (9*3*2) l
    | otherwise = putStrLn $ show $ extract_block' 0 $ drop (n*3) l

extract_block' _ [] = []
extract_block' count l 
    | count > 2 = []
    | otherwise = (take 3 l) ++ (extract_block' (count+1) $ drop 9 l)       



checkRepeat l = fst(foldr (\x seed -> (fst(seed) && not (x `elem` snd(seed)), x:snd(seed)) ) (True, []) l )