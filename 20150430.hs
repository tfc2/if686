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

push :: t -> Fila t -> Failable (t, Fila t)
push novoElemento (Element elemento (fila) capacidade)
    | capacidade < (contaElementos (Element elemento (fila) capacidade) 0) = Works (novoElemento, (adicionaElemento novoElemento (Element elemento (fila) capacidade)))
    | otherwise = Fail "Fila cheia"

contaElementos :: Fila t -> Int -> Int
contaElementos Nil tamanho = tamanho
contaElementos (Element elemento (fila) capacidade) tamanho = contaElementos (fila) (tamanho+1)

adicionaElemento :: t -> Fila t -> Fila t
adicionaElemento novoElemento (Element elemento Nil capacidade) = (Element elemento (Element novoElemento Nil capacidade) capacidade)