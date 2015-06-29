module Main where
import Data.Char
main :: IO ()
main = do contents <- getContents
          run $ map read (words$contents)

run (x:xs) = parse_instances x 1 $ take (x*9*9) xs

parse_instances 0 _ _ =  return()
parse_instances n n1 m = do putStrLn $ "Instance " ++ show(n1)
                            extract_lines $ take (9*9) m
                            parse_instances (n-1) (n1+1) $ drop (9*9) m


extract_lines [] = return ()
extract_lines l = do    putStrLn (show (take 9 l))
                        extract_lines $ drop 9 l

extract_columns [] = return ()
extract_columns l = do    putStrLn (show (take 9 l))
                        extract_columns $ drop 9 l
