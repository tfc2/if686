vendas :: Int -> Int
vendas n = n + 2

vendasEqual :: Int -> Int -> Int
vendasEqual s n 
    | n == -1 = 0
    | vendas n == s = (vendasEqual (s) (n-1)) + 1
    | otherwise = (vendasEqual (s) (n-1)) + 0