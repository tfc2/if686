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

-- Ex. 1: criarFila 5 0 -- Works (0,Element 0 Nil 5) 
-- Ex. 2: criarFila 0 4 -- Fail "Capacidade menor que 1"

push :: t -> Fila t -> Failable (t, Fila t)
push novoElemento (Element elemento (fila) capacidade)
    | capacidade > (contaElementos (Element elemento (fila) capacidade) 0) = Works (novoElemento, (adicionaElemento novoElemento (Element elemento (fila) capacidade)))
    | otherwise = Fail "Fila cheia"

-- Ex. 1: push 1 (Element 3 (Element 4 Nil 10) 10) -- Works (1,Element 3 (Element 4 (Element 1 Nil 10) 10) 10)
-- Ex. 2: push 1 (Element 3 (Element 4 Nil 2) 2) -- Fail "Fila cheia"

contaElementos :: Fila t -> Int -> Int
contaElementos Nil tamanho = tamanho
contaElementos (Element elemento (fila) capacidade) tamanho = contaElementos (fila) (tamanho + 1)

-- Ex. 1: contaElementos (Element 5 (Element 4 Nil 4) 4) 0 -- 2
-- Ex. 2: contaElementos Nil 0 -- 0

adicionaElemento :: t -> Fila t -> Fila t
adicionaElemento novoElemento (Element elemento Nil capacidade) = (Element elemento (Element novoElemento Nil capacidade) capacidade)
adicionaElemento novoElemento (Element elemento (fila) capacidade) = (Element elemento (adicionaElemento novoElemento fila) capacidade)

-- Ex. 1: adicionaElemento 4 (Element 5 (Element 3 Nil 4) 4) -- Element 5 (Element 3 (Element 4 Nil 4) 4) 4
-- Ex. 2:adicionaElemento 4 (Element 5 Nil 4) -- Element 5 (Element 4 Nil 4) 4