module Main where
import Data.Char
main :: IO ()
-- Função colocada somente para fosse feito o teste de compilação com o GHC -XFlexibleInstances
main = do contents <- getContents
          mapM_ putStrLn $ lines contents
-- 13.09
-- Só funciona com ghci -XFlexibleInstances
-- Não funciona com o GHCI se descomentar
-- instance (Ord a, Ord b) => Ord (a,b) where 
    -- (a, b) < (c, d) = a < c || (a==c && b < d)
-- 
-- instance Ord b => Ord [b] where
    -- _ < [] = False
    -- [] < _ = True
    -- (a:x) < (b:y) = a < b || (a==b && x < y)
-- 
--     
-- 13.11
-- Testar usando: showBoolFun (==True)
showBoolFun:: (Bool -> Bool) -> String 
showBoolFun func = "True->" ++ (show$ func True )  ++ "     False->" ++ (show$ func False)
-- Testar usando: showBoolFunGen show (==True)
showBoolFunGen:: (a -> String) -> (Bool  -> a) -> String
showBoolFunGen showElem func = "True->" ++ (showElem$func True) ++ "     False->" ++ (showElem$func False)

-- 13.12
-- Só funciona com ghci -XFlexibleInstances
-- Não funciona com o GHCI se descomentar
instance Show (Bool -> Bool) where    
    show func = "True->" ++ (show$ func True )  ++ "     False->" ++ (show$ func False)



-- 13.15 
data Roman = Roman Integer
data Tree = Tree Integer
-- Testar usando: map showRoman [1..91] 
showRoman :: Integer -> [Char]
showRoman n  
    | n <= 10 = romanMultiples n 'I' 'V' 'X'
    | n <= 100 = romanMultiples (div n 10) 'X' 'L' 'C' ++ showRoman (mod n 10)
    | n <= 1000 = romanMultiples (div n 100) 'C' 'D' 'M'  ++ showRoman (mod n 100)
    | n <= 1000 = romanMultiples (div n 100) 'C' 'D' 'M'  ++ showRoman (mod n 100)
    | n > 1000 = foldr (\x seed -> 'M':seed) [] [1..(div n 1000)]  ++ showRoman (mod n 1000)   
    | otherwise = ""

romanMultiples n minS midS maxS    
    | 1 <= n && n < 4 = foldr (\x seed -> minS:seed) [] [1..n]    
    | n == 4 = minS:midS:[]
    | n > 4 && n < 9  = midS:(romanMultiples (n-5) minS midS maxS)
    | n == 9  = minS:maxS:[]
    | n ==10  = maxS:[]
    | otherwise  =  ""


instance Show Roman where
    show (Roman n) = showRoman n

instance Num Roman where 
    (Roman x1) + (Roman x2) = Roman (x1+x2)
    (Roman x1) - (Roman x2) = Roman (x1-x2)
    (Roman x1) * (Roman x2) = Roman (x1*x2)
    abs(Roman x1) = Roman (abs x1)    
    signum(Roman x1) = Roman (signum x1)
    fromInteger x1 = Roman (fromInteger x1)


-- 13.21 
-- Can a function f::(a,[a]) -> a be applied to arguments like (2,[3]), (2,[]) and (2,[True]), 
-- if so what are the results 
--    (2,[3])
-- Sim, pode ser aplicada pq o segundo item da tupla é uma lista de 
-- tipos iguais ao tipo de retorno da função
-- Retorna Integer
--    (2,[]) - 
-- Sim, pode ser aplicada mesmo sendo uma lista vazia, isso não conflita com o tipo de
-- retorno da função
-- Retorna Integer
--    (2,[True])
-- Não o tipo da lista conflita com o tipo do retorno da função 
-- No instance for (Num Bool) arising from the literal `2'
func21 :: (a,[a]) -> a
func21 (a1, _) = a1


-- 13.26
-- Give the type of each of the individual conditional equations wich follow, 
-- and discuss the type of the function which together they define
merge' (x:xs) (y:ys)
-- merge' :: Ord a => [a] -> [a] -> [a]
    | x > y                     = x: merge' xs (y:ys)                          
-- x -> xs  -> y -> ys      (x:    merge xs       (y:ys                   ) -> [a] ) -> returns [a]
-- a -> [a] -> a -> [a] ->  (a -> (Ord a => [a] -> (a -> [a] -> [a]) -> [a]) -> [a]) -> [a]
    | x == y                    = x: merge' xs ys
-- x -> xs  -> y -> ys      (x:   (merge xs  ys  -> [a]      )       ) -> returns [a]
-- a -> [a] -> a -> [a] ->  (a -> (Ord a => [a] - [a]> -> [a]) -> [a]) -> [a]
    | otherwise     = y: merge' (x:xs) ys
-- x -> xs  -> y -> ys      (y:   (merge    (x:xs           ) -> ys  -> [a]) -> returns [a]
-- a -> [a] -> a -> [a] ->  (a -> (Ord a => (a -> [a] -> [a]) -> [a] -> [a]) -> [a]    
merge' (x:xs) []    = (x:xs) 
-- merge' [a] [] :: [a]
merge' [] (y:ys)    = (y:ys) 
-- merge' [] [a] :: [a]
merge' [] []        = []
-- merge' [] [] :: Ord a => [a]
