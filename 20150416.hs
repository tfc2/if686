-- Trabalho 8 --

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

-- Atividade de sala --

-- Questao 1 --

inverte :: (t -> u -> v) -> (u -> t -> v)
inverte f = (\ x y -> f y x)

-- Questao 2 --

primeirosPares :: [(t, u)] -> [t]
primeirosPares lista = map (\ (x,y) -> x) lista

-- Questao 3 --

comprimentoMaior :: [[Int]] -> Int -> [[Int]]
comprimentoMaior listas n = filter (\x -> (length x > n)) listas

-- Questao 4 --

uniao :: (Eq t) => (Ord t) => [[t]] -> [t]
uniao listas = removeRepeticao (sort ((foldr (\ x y -> x ++ y) [] listas)))

sort :: (Eq t) => (Ord t) => [t] -> [t]
sort [] = []
sort (a:as) = sort [b | b <- as, b < a] ++ [a] ++ sort [b | b <- as, b >= a]

removeRepeticao :: (Eq t) => [t] -> [t]
removeRepeticao [] = []
removeRepeticao (a:as)
    | (as /= []) && (a == head as) = removeRepeticao as
    | otherwise = a : removeRepeticao as

-- resolucao de Castor: 
uniao' :: (Eq t) => [[t]] -> [t]
uniao' listas = foldr (\x y -> ([a | a <- x, (length [b |b <-y, a == b] == 0 )]) ++ y) [] listas

-- Questao 6: mapfold --

-- Questao 7 --

somaNumero :: [Int] -> Int -> [Int]
somaNumero lista n = map (+ n) lista

-- Questao 8 --

maiorNumero :: [Int] -> Int
maiorNumero lista = foldr (max) 0 lista
