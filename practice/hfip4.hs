asc :: Int -> Int -> [Int]
asc n m
    | n > m = []
    | n == m = [m]
    | n < m = n : asc (n+1) m