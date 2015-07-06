-- 2012-p3
-- fa = concat . map show 
-- show :: Show a => a -> String
-- map :: (a -> b) -> [a] -> [b]
-- concat :: [[a]] -> [a]
-- map show :: Show a => (a -> String) -> [a] -> [String]
-- concat . map show :: Show a => [a] -> String
-- app2 f g x = (f x , g x)
-- app2 :: (a -> b) -> (a -> c) -> a-> (b, c)
-- maybe z f Nothing = z 
-- maybe:: a -> (a->b) -> Maybe a -> a
-- maybe z f (Just x) = f x
-- maybe:: a -> (a -> b) -> Maybe c -> (a -> c -> d) -> d

data Tree a = EmptyTree | Node a (Tree a) (Tree a)
    deriving (Show)

sampleTree = Node 4 (Node 1 EmptyTree EmptyTree) (Node 5 EmptyTree EmptyTree)
--        4
--    1       5
bigTree = Node 4 (Node 1 EmptyTree (Node 3 EmptyTree EmptyTree)) (Node 5 EmptyTree EmptyTree)
--        4
--    1       5
--      3       
insert:: Ord a => a -> Tree a -> Tree a
insert e EmptyTree = Node e EmptyTree EmptyTree
insert e tree@(Node v t1 t2) 
    | e < v = Node v (insert e t1) t2
    | e > v = Node v t1 (insert e t2)
    | otherwise = tree

search:: Ord a => a -> Tree a -> Bool
search e EmptyTree = False
search e tree@(Node v t1 t2) 
    | e < v = search e t1
    | e > v = search e t2
    | otherwise = True

delete:: Ord a => a -> Tree a -> Tree a
delete e EmptyTree = EmptyTree
delete e tree@(Node v t1 t2) 
    | e < v = Node v (delete e t1) t2
    | e > v = Node v t1 (delete e t2)
    | otherwise = join t1 t2

join t1 EmptyTree  = t1 -- Minor from right
join t1 t2  = Node m t1 t2' -- m = value from the minor on the right side 
    where (m, t2') = minorFromRight t2

minorFromRight:: Ord a => Tree a -> (a, Tree a)
minorFromRight (Node v EmptyTree t) = (v, t)
minorFromRight t@(Node v t1 t2) = (v', Node v t1' t2) 
                            where (v', t1') = minorFromRight t1







-- insert e EmptyTree = Node e EmptyTree EmptyTree
-- -- @ -- at pattern
-- insert e t@(Node v t1 t2) 
    -- | e < v = Node v (insert e t1) t2
    -- | e > v = Node v t1 (insert e t2)
    -- | otherwise = t
--     
-- search:: Ord a => a -> Tree a -> Bool
-- search e EmptyTree = False
-- search e t@(Node v t1 t2)
    -- | e < v = search e t1
    -- | e > v = search e t2
    -- | otherwise  = True
--     
--     
-- delete:: Ord a => a -> Tree a -> Tree a
-- delete e EmptyTree = EmptyTree
-- -- @ -- at pattern
-- delete e t@(Node v t1 t2)
    -- | e < v = Node v (delete e t1) t2
    -- | e > v = Node v t1 (delete e t2)
    -- | otherwise  = join t1 t2
-- 
-- join t1 EmptyTree  = t1 
-- join t1 t2  = Node m t1 t2'
    -- where (m, t2') = minor t2
-- 
-- minor:: Ord a => Tree a -> (a, Tree a)
-- minor (Node v EmptyTree t) = (v, t)
-- minor t@(Node v t1 t2) = (v', Node v t1' t2) where (v', t1') = minor t1
-- --Node 2 (Node 1 EmptyTree EmptyTree) (Node 4 (Node 3 EmptyTree EmptyTree) (Node 5 EmptyTree EmptyTree))