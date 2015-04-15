-- Trabalho 8

qsort :: Ord a => Num a => [a] -> [a]
qsort [] = []
qsort (a:as) = qsort menor ++ [a] ++ qsort maior
    where menor  = [ x | x <- as, x < a ]
          maior = [ x | x <- as, x >= a ]

filtro :: Ord a => Num a => [a] -> [a] -> [[a]]
filtro (a:[]) bs = [qsort (filter (<=a) bs)] ++ [qsort((filter (>a) bs))]
filtro (a:as) bs = [qsort (filter (<=a) bs)] ++ filtro as (filter (>a) bs)

listPartitioner :: Ord a => Num a => [a] -> ([a] -> [[a]])
listPartitioner as = filtro (qsort as)