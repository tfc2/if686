--- Trabalho 11 ---

-- Questao 1 --

data Table = Somente [(Int, Int)] | Nada -- Primeiro int = chave, segundo int = valor

instance Monad Table where
 (>>=) (Somente x) f = f x
 (>>=) Nada _ = Nada
 return Somente x = Somente x

getElem :: Table -> Int -> Table
getElem Nothing _ = Nada
getElem ((a, as):bs) n
 | a == n = Somente [as]
 | otherwise = Somente getElem bs n

hasKey :: Table -> Int -> Bool
hasKey Nothing _ = False
hasKey ((a, as):bs) n
 | a == n = True
 | otherwise = hasKey bs n

putElem :: Table -> (Int, Int) -> Table
putElem as (x, y) 
 | hasKey as x = Somente as -- Nao adiciona se ja existir a chave
 | otherwise = Somente (as ++ [(x,y)])

removeElem :: Table -> Int -> Table -- int = chave
removeElem Nothing _ = Nada
removeElem xs x = Somente [(a, as) | (a, as) <- xs, (a /= x)]