{-
Exercise #1:
Create a function elementOf that returns true if an element is in a given list, 
    and returns false otherwise.
-}

asc :: Int -> Int -> [Int]
asc n m
    | n > m = []
    | n == m = [m]
    | n < m = n : asc (n+1) m

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