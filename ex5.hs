-- 10.13
sumsquares l = foldr (+) 0 $ map (^2) l
-- 10.14
sumPosSquares l = foldr (+) 0 $ map (^2) $ filter (>0) l
-- 10.15 
lastone l = foldr (\x seed -> if (seed == []) then [x] else seed) [] l
inits l = snd $ foldr (\x seed -> if (fst(seed) == True) then (False,[]) else (fst(seed), x:snd(seed)) ) (True, [])  l
-- 10.20
addTen x =  x + 10
addOne x =  x + 1
switchMaps f1 f2 l = snd $ foldl (\seed x -> if (fst(seed) == True) then (False, snd(seed) ++ [f1(x)]) else (True, snd(seed) ++ [f2(x)]) )  (True, []) l

-- 10.21
split l = snd $ foldl (\seed x -> if (fst(seed) == True) then (False, (fst(snd(seed)) ++ [x], snd(snd(seed))) )  else (True, ( fst(snd(seed)), snd(snd(seed)) ++ [x] ) ) )  (True, ([],[])) l
merge1 ([],[]) = []
merge1 (l1,[]) = l1
merge1 ([],l2) = l2
merge1 (x1:l1, x2:l2) = x1:x2:merge1(l1,l2)

