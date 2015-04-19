------ TRABALHO 9 ------

------ Questao 1 ------

-- 20150319 --

-- 1. Fibonacci

fib :: Int -> Int
fib n
    | n < 2 = n
    | otherwise = (fib (n-1)) + (fib (n-2))

fibAux :: Int -> Int -> [Int]
fibAux 0 _ = []
fibAux n x 
    | ((mod fibonacci 2) == 0) = fibonacci : fibAux (n-1) (x+1)
    | otherwise = fibAux n (x+1)
 where fibonacci = (fib x)

fibPar :: Int -> [Int]
fibPar n = fibAux n 0

-- 2. Ordena a soma

qsortDigits :: [Int] -> [Int]
qsortDigits [] = []
qsortDigits (a:as) = qsortDigits menor ++ [a] ++ qsortDigits maior
    where menor  = [ x | x <- as, sumDigits x < sumDigits a ]
          maior = [ x | x <- as, sumDigits x >= sumDigits a ]

sumDigits :: Int -> Int
sumDigits 0 = 0
sumDigits x = sumDigits (x `div` 10) + (x `mod` 10)

ordenar :: [Int] -> [Int]
ordenar as = qsortDigits as

-- 20150324 --

-- 1. Processamento de Texto
getWord :: String -> String
getWord [] = []
getWord (a:as)
    | a /= ' ' = a : getWord as
    | otherwise = []

dropWord :: String -> String
dropWord [] = []
dropWord (a:as)
     | a /= ' ' = dropWord as
     | otherwise = (a:as)

dropSpace :: String -> String
dropSpace [] = []
dropSpace (a:as)
    | a == ' ' = dropSpace as
    | otherwise = (a:as)

type Word = String

quebraEspaco :: String -> String
quebraEspaco [] = []
quebraEspaco (a:as)
    | a == ' ' = []
    | otherwise = a : quebraEspaco as

splitWords :: String -> [Word]
splitWords [] = []
splitWords (a:as) 
    | a /= ' ' = (getWord (a:as) : splitWords (dropWord (a:as)))
    | otherwise = splitWords as

type Line = [Word]

getLine' :: Int -> [Word] -> Line
getLine' 1 (a:as) = [a]
getLine' n (a:as) = getLine' (n-1) as

dropLine :: Int -> [Word] -> [Word]
dropLine 1 (a:as) = as
dropLine n (a:as) = a : dropLine (n-1) as

-- 20150326 --

-- 1. Função agrupar
contaNumaLista :: Eq t => Int -> t -> [t] -> Int
contaNumaLista n _ [] = n 
contaNumaLista n x (a:as)
    | x == a = contaNumaLista (n+1) x as
    | otherwise = contaNumaLista n x as

conta :: Eq t => Int -> t -> [[t]] -> Int
conta n x [] = n
conta n x (a:as) = conta (n + (contaNumaLista 0 x a)) x as

retirar :: Eq t => t -> [[t]] -> [[t]]
retirar x [] = []
retirar x ([]:as) = retirar x as
retirar x ((b:bs):as)
    | x == b = retirar x (bs:as)
    e| otherwise = [b] : retirar x (bs:as)

agrupar :: Eq t => [[t]] -> [(t, Int)]
agrupar [] = []
agrupar ([]:as) = agrupar as
agrupar ((b:bs):as) = (b, (conta 0 b ((b:bs):as))) : agrupar (retirar b ((b:bs):as))

-- 20150407 --

-- 2. Defina as seguintes funções:

data List t = Nil | Cabeca t (List t) deriving (Show)

toList :: List t -> [t]
toList Nil = []
toList (Cabeca x (y)) = [x] ++ (toList y)

fromList :: [t] -> List t
fromList [] = Nil
fromList (a:as) = (Cabeca a (fromList as))

data Tree t = NilT | Node t (Tree t) (Tree t) deriving (Eq, Show)

depth :: Tree t -> Int
depth NilT = 0
depth t = percorre t (-1) -- raiz tem depth 0

percorre :: Tree t -> Int -> Int
percorre NilT n = n
percorre (Node v e d) n = max (percorre e (n+1)) (percorre d (n+1))

-- depth (Node 1 (Node 2 (Node 3 NilT NilT) (Node 4 NilT NilT)) (Node 5 NilT (Node 5 (Node 5 (Node 5 (Node 6 NilT NilT) NilT) NilT) NilT)))

bfs :: Eq t => Tree t -> t -> Bool
bfs NilT n = False
bfs (Node v e d) n
 | v == n = True
 | otherwise = (||) (bfs e n) (bfs d n)
 
mapTree :: (t -> u) -> Tree t -> Tree u
mapTree f NilT = NilT
mapTree f (Node v e d) = (Node (f v) (mapTree f e) (mapTree f d))

----- Questo 2 -----

data Grafos = Grafo [(Int, [(Int,Double)])] deriving (Show, Eq) -- grafo representado por vertice e lista de (adjacencente, peso)
-- foi definido na questao que o rotulo seria obrigatoriamente um int, entao fizemos uma modificacao para tirar tambem o NilG
-- alem disso, precisamos alterar o valor dos pesos para um Double, para utilizar o algoritmo de djkstra que inicia distancias com um infinito (Double)
-- para um grafo não direcionado assumimos que na entrada se eu tiver uma adjacencia de numero1 para numero2, teremos tambem de numero2 para numero1 
-- para facilitar o algoritmo consideramos uma tabela = vertice, distancia minima para chegar ate ele, precedente
-- referencia: http://www.inf.ufsc.br/grafos/temas/custo-minimo/dijkstra.html

g = Grafo ([(0,[(1,10),(2,2)]), (1,[(0,10),(2,4),(3,5)]), (2,[(0,2),(1,4),(4,3)]), (3,[(1,5),(4,2)]), (4,[(2,3),(3,2)])]) -- exemplo

infinito = 1/0

formaTabela :: Grafos -> Int -> [(Int, Double, Int)] -- forma a tabela usada no algoritmo colocando a distancia do vertice inicial = 0
formaTabela (Grafo []) _ = []
formaTabela (Grafo ((x,y):as)) inicial
    | x == inicial = (x, 0, x) : (formaTabela (Grafo as) inicial) -- se for o vertice inicial, a distancia sera 0
    | otherwise = (x, infinito, (-1)) : (formaTabela (Grafo as) inicial) -- ao contrario, a principio distancia = infinito e o precedente = -1

defineAbertos :: Grafos -> [Int] -- pega os vertices do grafo para a primeira definicao de quais estao abertps
defineAbertos (Grafo []) = []
defineAbertos (Grafo ((vertice, y):as)) = vertice : (defineAbertos (Grafo (as)))

marcaFechado :: [Int] -> (Int, Double) -> [Int] -- marca um vertice como fechado
marcaFechado abertos (vertice, distanica) = [x | x <- abertos, x /= vertice]

menorDistancia :: [(Int, Double, Int)] -> [Int] -> Double -- retorna a menor distancia existente na tabela entre os vertices abertos
menorDistancia tabela abertos = foldr (min) infinito ([y | (x,y,z) <- tabela, (elem x abertos)])

verticeMenorDistancia :: [(Int, Double, Int)] -> [Int] -> (Int, Double)
verticeMenorDistancia tabela abertos = head ([(x,y) | (x,y,z) <- tabela, y == (menorDistancia tabela abertos)])

adjacentes :: Grafos -> (Int,Double) -> [(Int, Double)] -- retorna a lista de vertices adjacentes e seus pesos usando o grafo como entrada
adjacentes (Grafo []) _ = []
adjacentes (Grafo ((x,y):as)) (vertice,peso)
    | x == vertice = y
    | otherwise = adjacentes (Grafo as) (vertice,peso)

auxTabela :: [(Int, Double, Int)] -> (Int, Double) -> [Int] -> (Int, Double) -> [(Int, Double, Int)] -- altera distancia e precedente caso melhor caminho
auxTabela [] _ _ _ = []
auxTabela ((vertice1, distanciaAtual, precendete):as) (anterior, distancia) abertos (vertice2,peso)
    | (vertice1 == vertice2) && (elem vertice1 abertos) && ((peso + distancia) < distanciaAtual) =  ((vertice1, (peso + distancia), anterior):as)
    | otherwise = (vertice1, distanciaAtual, precendete) : (auxTabela as (anterior, distancia) abertos (vertice2,peso))

alteraTabela :: [(Int, Double, Int)] -> (Int, Double) -> [Int] -> [(Int, Double)] -> [(Int, Double, Int)] -- atualiza os adjacentes usando auxTabela
alteraTabela _ _ _ [] = []
alteraTabela tabela verticeAnterior abertos ((vertice,peso):as)
    | as == [] = auxTabela tabela verticeAnterior abertos (vertice,peso)
    | otherwise = alteraTabela (auxTabela tabela verticeAnterior abertos (vertice,peso)) verticeAnterior abertos as

formaCaminho :: [(Int, Double, Int)] -> [(Int, Double, Int)] -> Int -> Int -> [Int] -- forma o caminho a partir dos precedentes
formaCaminho [] _ _ _ = []
formaCaminho ((vertice, distancia, precedente):as) tabelaOficial inicio posicao
    | posicao == inicio = [posicao]
    | vertice == posicao = (formaCaminho tabelaOficial tabelaOficial inicio precedente) ++ [vertice]
    | otherwise = formaCaminho as tabelaOficial inicio posicao

dijkstra :: Grafos -> [(Int, Double, Int)] -> [Int] -> (Int, Double) -> [(Int, Double, Int)] -- algoritmo de dijkstra
dijkstra grafo tabela abertos anterior
	| ((length abertos) == 1) = alteraTabela tabela anterior abertos (adjacentes grafo (verticeMenorDistancia tabela abertos)) 
	| otherwise = dijkstra grafo (alteraTabela tabela anterior abertos (adjacentes grafo (verticeMenorDistancia tabela abertos))) (marcaFechado abertos (verticeMenorDistancia tabela abertos)) (verticeMenorDistancia (alteraTabela tabela anterior abertos (adjacentes grafo (verticeMenorDistancia tabela abertos))) (marcaFechado abertos (verticeMenorDistancia tabela abertos)))
 
geraFuncaoMenorCaminho :: Grafos -> (Int -> Int -> [Int]) -- por enquanto nao em forma de funcao
geraFuncaoMenorCaminho grafo inicio fim = formaCaminho (dijkstra grafo (formaTabela grafo inicio) (defineAbertos grafo) (inicio, 0.0)) (dijkstra grafo (formaTabela grafo inicio) (defineAbertos grafo) (inicio, 0.0)) inicio fim