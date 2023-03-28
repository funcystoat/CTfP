asc :: Int -> Int -> [Int]
asc n m
    | n > m = []
    | n == m = [m]
    | n < m = n : asc (n+1) m

{-
Exercise #1:
Create a function elementOf that returns true if an element is in a given list, 
    and returns false otherwise.
-}

-- My solution
elementOf :: (Eq a) => a -> [a] -> Bool
elementOf x xs
    | null xs = False
    | x == head xs = True
    | x /= head xs = elementOf x (tail xs)

-- proposed solution
elementOf2 :: (Eq a) => a -> [a] -> Bool
elementOf2 _ [] = False
elementOf2 e (x:xs) = (e == x) || elementOf2 e xs

{-
Create a nubby function that removes all duplicates from a given list
-}
-- My solution (Was the same as the proposed solution)
nuby :: (Eq a) => [a] -> [a]
nuby [] = []
nuby (x:xs)
    | elementOf x xs = nuby xs
    | otherwise = x : nuby xs

{-
Create a function isAsc which takes a list and returns true if
    the list is in ascending order, and false otherwise
-}
-- My solution
isAsc :: (Ord a) => [a] -> Bool
isAsc [] = True
isAsc [x] = True
isAsc (x:xs) = (x <= head xs) && isAsc xs

-- Proposed solution (letting head xs be y since we KNOW it is there anyway)
isAsc2 :: (Ord a) => [a] -> Bool
isAsc2 [] = True
isAsc2 [x] = True
isAsc2 (x:y:xs) = (x <= y) && isAsc2 (y:xs)

{-
Create a function hasPath that takes a DiGraph represented as
a list of 2-tuples, a start node, an end node, and determines whether
or not there exists a path from the start node to the end node.

Ex: [(1,2), (2,3), (3,2), (4,3), (4,5), (3,4)]
-}
-- My Solution
-- The majority of my solution utilizes this "squish" function.
-- squish recursively generates every tuple of nodes with paths between them
-- it does so by joining any two tuples on their commen end-start nodse
squish :: (Eq a) => [(a,a)] -> [(a,a)]
squish [] = []
squish xs 
    | null z = nuby (z ++ xs)
    | otherwise = nuby (squish (z ++ xs))
    where z = [(fst x, snd y) | (x,y) <- xs, snd x == fst y, not(elementOf (fst x, snd y) xs)]

hasPath :: [(Int, Int)] -> Int -> Int -> Bool
hasPath xs start end 
    | start == end = True
    | otherwise = elementOf (start, end) (squish xs)

-- propose solution
hasPath2 :: [(Int, Int)] -> Int -> Int -> Bool
hasPath2 [] x y = x == y
hasPath2 xs x y
    | x == y = True
    | otherwise =
        let xs' = [(n,m) | (n,m) <- xs, n /= x] in
            or [hasPath2 xs' m y | (n,m) <- xs, n == x]
