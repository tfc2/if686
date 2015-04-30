data Failable t = Fail String | Works t deriving Show

data Fila t = Element t (Fila t) Int | Nil deriving Show

instance Monad Failable where
 (>>=) (Works x) f = f x
 (>>=) (Fail x) _ = Fail x
 return x = Works x

criarFila :: Int -> t -> Failable (t, Fila t)
criarFila capacidade primeiroElemento
    | capacidade < 1 = Fail "Capacidade menor que 1"
    | otherwise = Works (primeiroElemento, (Element primeiroElemento (Nil) capacidade))