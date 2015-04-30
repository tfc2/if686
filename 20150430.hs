data Failable t = Fail String | Works t

data Fila t = Element t (Fila t) | Nil

instance Monad Failable where
 (>>=) (Works x) f = f x
 (>>=) (Fail x) _ = Fail x
 return x = Works x

criarFila :: Int -> t -> Failable (t, Fila t)
criarFila capacidade primeiroElemento = Works (primeiroElemento, (Element primeiroElemento (Nil)))