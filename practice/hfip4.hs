asc :: Int -> Int -> [Int]
asc n m
    | n > m = []
    | n == m = [m]
    | n < m = n : asc (n+1) m

my_sum :: [Int] -> Int
my_sum [] = 0
my_sum (x:xs) = x + my_sum xs

addTuples :: [(Int, Int)] -> [Int]
addTuples xs = [x+y | (x,y) <- xs]