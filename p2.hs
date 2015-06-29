len' :: [a] -> Integer
len' = foldr(\x seed -> seed + 1) 0
lenl' = foldl (\seed x -> seed+1) 0

--remDupsR :: Eq a => [a] -> [a]
--remDupsR x = (foldr insert x [])
	--where insert x [] = [x]
	      --insert x (y:z) = if(x /= y) then (x:y:z) else (y:z)

--remDupsR :: Eq a => [a] -> [a]
--remDupsR x = (foldr insert x [])
	--where insert x [] = [x]
	      --insert x (y:z) = if(x /= y) then (x:y:z) else (y:z)


-- take care with the parentesis and the sides
-- foldr x [] -- x:tl
-- foldl [] x -- tl++[x]
remdupr :: Eq a => [a] -> [a]
remdupr = foldr remdupr1 [] 
    where   remdupr1 x [] = [x]
            remdupr1 x (h:tl) = if (x == h) then (h:tl) else (x:h:tl)

remdupl :: Eq a => [a] -> [a]
remdupl = foldl remdupl1 [] 
    where   remdupl1 [] x  = [x]
            remdupl1 (h:tl) x = if (x == h) then (h:tl) else (x:h:tl)

nub :: Eq a => [a] -> [a]
nub = foldr nub' []
    where   nub' x [] = [x]
            nub' x l = if (notin x l) then (x:l) else l
                where   notin x' [] = True
                        notin x' (h:tl) = if (x' == h) then False else (notin x' tl)

nubl :: Eq a => [a] -> [a]
nubl = foldl nubl' []
    where   nubl' [] x = [x]
            nubl' l  x = if (notin x l) then (l++[x]) else l
                where   notin x' [] = True
                        notin x' (h:tl) = if (x' == h) then False else (notin x' tl)

fat 0 = 1
fat 1 = 1
fat n = n * fat (n-1)

-- approxe n = sum(i=1..n) 1/i!
-- approxe :: Integer -> Integer
approxe 0 = 1
approxe 1 = 1
approxe n = snd ( foldl appr' (1,0) [1..n] ) 
    where appr' (i, r) x = (x*i, r + (1/(x*i)))

takewhile' :: (a->Bool) -> [a] -> [a]
takewhile' f = foldr (\x seed -> if f x then x:seed else seed) []
--takewhile' p = foldl (\ys x -> if p x then ys ++ [x] else ys) []