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
elementOf2 e (x:xs) = (e == x) || (elementOf2 e xs)

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