{-Trabalho 3-}

{-Questao 1-}

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