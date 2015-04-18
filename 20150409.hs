----------- TRABALHO 06 -------------

----------- questao 1 -------------

data Grafos t = Nil | Grafo [(t, [(t,Int)])] deriving (Show, Eq) -- grafo representado por vertice e lista de (adjacencente, peso)

{-
Ex. 1: show (Grafo [(1,[(2,1),(3,2)]), (2,[(1,3),(3,1)]), (3,[(2,2)])]) -- "Grafo [(1,[(2,1),(3,2)]),(2,[(1,3),(3,1)]),(3,[(2,2)])]"
Ex. 2: (Grafo [(1,[(1,1),(2,1)]), (2,[(1,2)])]) == (Grafo [(1,[(1,1),(2,1)]), (2,[(1,2)])]) -- True
Ex. 3: (Grafo [(1,[(1,1),(2,1)]), (2,[(1,2)])]) == (Grafo [(1,[(2,1)]), (2,[(1,2)])]) -- False
-}

----------- questao 2 -------------

{-
Ex. 1: search (Grafo [(1,[(2,1),(3,2)]), (2,[(1,3),(3,1)]), (3,[(2,2),(4,3)]), (4,[(2,2)]), (5,[])]) 1 4 -- True
Ex. 2: search (Grafo [(1,[(2,1),(3,2)]), (2,[(1,3),(3,1)]), (3,[(2,2),(4,3)]), (4,[(2,2)]), (5,[])]) 1 5 -- False
-}

listaVertices :: (Eq t) => Grafos t -> [(t,Bool)] -- constroi a lista de vertices com a flag booleana "visitado" = False, usando o grafo como entrada
listaVertices Nil = []
listaVertices (Grafo ((x,y):as)) = [(x,False)] ++ (listaVertices (Grafo as))

marcaVertices :: (Eq t) => [(t,Bool)] -> t -> Bool -> [(t,Bool)] -- altera "visitado" usando como entrada a lista de vertices, o vertice e True or False
marcaVertices [] vertice visitado = []
marcaVertices ((x,y):as) vertice visitado
    | x == vertice = [(x,visitado)] ++ (marcaVertices as vertice visitado)
    | otherwise = [(x,y)] ++ (marcaVertices as vertice visitado)

pegaVertices :: [(t,Int)] -> [t]
pegaVertices [] = []
pegaVertices ((vertice, peso):as) = vertice : (pegaVertices as)

adjacentes :: (Eq t) => Grafos t -> t -> [t] -- retorna a lista de vertices adjacents a um vertice t usando o grafo como entrada
adjacentes Nil vertice = []
adjacentes (Grafo ((x,y):as)) vertice
    | x == vertice = pegaVertices y
    | otherwise = adjacentes (Grafo as) vertice

visitado :: (Eq t) => [(t,Bool)] -> t -> Bool -- retorna o estado de "visitado" de um vértice usando a lista de vertices como entrada
visitado [] vertice = False
visitado ((x,y):as) vertice
    | x == vertice = y
    | otherwise = visitado as vertice

proximoAdjacente :: (Eq t) => [t] -> [(t,Bool)] -> [t] -- retorna o proximo vertice adjacente não visitado, entradas: listas de adjacentes e de vertices
proximoAdjacente [] vertices = []
proximoAdjacente (a:as) vertices
    | visitado vertices a == True = proximoAdjacente as vertices
    | otherwise = [a]

busca :: (Eq t) => Grafos t -> [(t,Bool)] -> [t] -> t -> t -> Bool -- funcao de busca em profundidade
busca (Grafo grafo) vertices [] inicio fim = False -- caso base de a pilha estar vazia (terem acabado os vertices adjacentes não visitados)
busca (Grafo grafo) vertices pilha inicio fim
    | (proximoAdjacente (adjacentes (Grafo grafo) inicio) vertices) == [] = busca (Grafo grafo) vertices (tail pilha) inicio fim -- não há adjacente válido, volta a pilha
    | head (proximoAdjacente (adjacentes (Grafo grafo) inicio) vertices) == fim = True -- chegou no vertice pretendido, retorna o caminho
    | otherwise = busca (Grafo grafo) (marcaVertices vertices inicio True) ((proximoAdjacente (adjacentes (Grafo grafo) inicio) vertices)++pilha) (head (proximoAdjacente (adjacentes (Grafo grafo) inicio) vertices)) fim 
    -- marca o proximo vertice adjacente como visitado, o coloca na pilha e o considera como inicio

search :: (Eq t) => Grafos t -> t -> t -> Bool -- funcao inicial que marca vertices como não lidos, define o incial como visitado e o coloca na fila
search (Grafo grafo) inicio fim = busca (Grafo grafo) (marcaVertices (listaVertices (Grafo grafo)) inicio True) [inicio] inicio fim

----------- Atividades de sala -------------

import Data.Char (ord)

-- a primeira questao foi alterada em sala para ao inves de calcular o quadrado, calcular a raiz
sqrList :: [Float] -> [Float]
sqrList lista = map sqrt lista

-- segunda questao
posicaoAlfabeto :: String -> [Int]
posicaoAlfabeto palavra = subAscii (map ord palavra)

subAscii :: [Int] -> [Int]
subAscii [] = []
subAscii (a:as) = a-96 : subAscii as

-- terceira questao
map' :: (t -> u) -> [t] -> [u]
map' f lista = [f a | a <- lista]

-- quarta questao
member :: (Eq t) => [t] -> t -> Bool
member lista elemento = foldr (||) False (map ((==) elemento) lista)

-- quinta questao
union' :: (Eq t) => [t] -> [t] -> [t]
union' lista1 lista2 = foldr (++) [] ([lista1]++([diferentes lista1 lista2]))

diferentes :: (Eq t) => [t] -> [t] -> [t]
diferentes _ [] = []
diferentes lista1 (b:bs)
    | member lista1 b = diferentes lista1 bs
	| otherwise = b : diferentes lista1 bs

-- sexta questao
listaSoma :: [String] -> [Int]
listaSoma palavras = map somaCaracteres palavras

somaCaracteres :: String -> Int
somaCaracteres palavra = foldr (+) 0 (posicaoAlfabeto palavra)
