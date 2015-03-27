-- Trabalho 3 --

type Table = [(Int, Int)] -- Primeiro int = chave, segundo int = valor

{-getElem :: Table -> Int -> Int
getElem [] _ = 0 -- ???
getElem ((a, as):bs) n
 | a == n = as
 | otherwise = getElem bs n-}

getElem :: Table -> Int -> [Int]
getElem [] _ = []
getElem ((a, as):bs) n
 | a == n = [as]
 | otherwise = getElem bs n

hasKey :: Table -> Int -> Bool
hasKey [] _ = False
hasKey ((a, as):bs) n
 | a == n = True
 | otherwise = hasKey bs n

putElem :: Table -> (Int, Int) -> Table
putElem as (x, y) 
 | hasKey as x = as -- Nao adiciona se ja existir a chave
 | otherwise = (as ++ [(x,y)])

removeElem :: Table -> Int -> Table -- int = chave
removeElem [] _ = []
removeElem xs x = [(a, as) | (a, as) <- xs, (a /= x)]

{-Questao 2-}

contemElem :: (Eq t) => [t] -> t -> Bool
contemElem [] _ = False
contemElem (a:as) b
 | a == b = True
 | otherwise = contemElem as b 

contem :: (Eq t) => [t] -> [t] -> Bool
contem [] _ = True -- o segundo conj contem todos os elems do primeiro
contem (a:as) b
 | contemElem b a = contem as b -- se o segundo conj contem aquele elemento, passa pro proximo
 | otherwise = False

intersec :: (Eq t) => [t] -> [t] -> Bool -- o primeiro conj contem algum elemento do segundo?
intersec _ [] = False
intersec (a:as) (b:bs)
 | contemElem (a:as) b = True
 | otherwise = intersec (a:as) bs

comparaConjuntos :: (Eq t) => [t] -> [t] -> String
comparaConjuntos a b
 | (contem a b) && (contem b a) = "A igual a B"
 | contem b a = "A contem B"
 | contem a b = "B contem A"
 | intersec a b = "A interseciona B"
 | otherwise = "Conjuntos disjuntos"

-- exercicios da aula --

take' :: [t] -> Int -> [t]
take' [] _ = [] -- caso a lista seja vazia
take' _ 0 = [] -- caso base
take' (a:as) n = a : (take' as (n-1)) -- incluindo os elementos ate n =1

drop' :: [t] -> Int -> [t]
drop' [] _ = [] -- caso a lista seja vazia
drop' (a:as) n
    | n == 0 = a : (drop' as n) -- caso ja tenha retirado n elementos
    | otherwise = drop' as (n-1) -- retirando os n primeiro elementos

takeWhile' :: (t->Bool) -> [t] -> [t]
takeWhile' _ [] = [] -- caso a lista seja vazia
takeWhile' condition (a:as) 
    | condition a = a : (takeWhile' condition as) -- respeita a condicao
    | otherwise = takeWhile' condition as -- pula o elemento que nao respeita

dropWhile' :: (t->Bool) -> [t] -> [t]
dropWhile' _ [] = [] -- caso a lista seja vazia
dropWhile' condition (a:as) 
    | condition a = dropWhile' condition as -- respeita a condicao
    | otherwise = a: (dropWhile' condition as) -- adiciona o elemento que nao respeita

putOrder :: Ord t => [t] -> [t]
putOrder [] = [] -- caso base
putOrder (a:as) = putOrder ([x | x <- as, x <= a]) ++ [a] ++ putOrder ([y | y <- as, y > a]) -- quicksort