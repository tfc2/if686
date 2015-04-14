----------- TRABALHO 07 -------------

-- Questao 1 --

junta :: (t -> t) -> (t -> t) -> (t -> t)
junta g f = f'
 where f' x = g (f x)

compose :: (t -> t) -> [(t -> t)] -> [(t -> t)]
compose g f = map (junta g) f

-- Questao 2 --

data Grafos t = Nil | Grafo [(t, [(t,Int)])] deriving (Show, Eq) -- grafico representado por vertice e lista de (adjacencente, peso)

g = Grafo ([(1,[(1,1),(2,1)]), (2,[(1,2)])]) -- exemplo

-- map --

mapGraph :: (Eq t) => (t -> u) -> Grafos t -> [(u, [(u,Int)])] -- aplica o map nos vertices
mapGraph f Nil = []
mapGraph f (Grafo ((vertice, adjacencias):as))
    | as == [] = ((f vertice), (mapAdjacencias f adjacencias)) : (mapGraph f Nil)
    | otherwise = ((f vertice), (mapAdjacencias f adjacencias)) : (mapGraph f (Grafo (as)))

mapAdjacencias :: (t -> u) -> [(t,p)] -> [(u,p)] -- retorna a lista de adjacencias com as funcoes aplicadas nos vertices
mapAdjacencias f [] = []
mapAdjacencias f ((vertice,elemento):as) = ((f vertice), elemento):(mapAdjacencias f as)
-- exemplo: mapGraph ((+)1) g -- [(2,[(2,1),(3,1)]),(3,[(2,2)])]

-- fold --

foldVertices :: (t -> u -> u) -> u -> [t] -> u -- aplica o fold na lista de vertices
foldVertices f u [] = u
foldVertices f u vertices = foldr f u vertices


pegaVertices :: (Eq t) => Grafos t -> [t] -- da "fold" no grafo para pegar so os vertices
pegaVertices Nil = []
pegaVertices (Grafo ((vertice, adjacencias):as))
    | as == [] = vertice : (pegaVertices Nil)
    | otherwise = vertice : (pegaVertices (Grafo (as)))

foldGraph :: (Eq t) => (t -> u -> u) -> u -> Grafos t -> u -- fold graph usando dois metodos acima
foldGraph f u (Grafo grafo) =  foldVertices f u (pegaVertices (Grafo (grafo)))

-- exemplo: foldGraph (max) 0 g -- 2
-- exemplo: foldGraph (+) 0 g -- 3

-- Questao 3 --

data Tree t = NilT | Node t (Tree t) (Tree t) deriving (Eq, Show)

--(Node 5 (Node 7 (Node 15 NilT (Node 6 NilT NilT)) (Node 2 NilT NilT)) (Node 10 NilT NilT))

filtro :: Eq t => (t -> Bool) -> Tree t -> (Tree t, [Tree t])
filtro _ NilT = (NilT, [])
filtro f (Node v e d)
    | f v = ((Node v esquerda direita), (listaEsq ++ listaDir))
    | not (f v) && (e == NilT) && (d == NilT) = (NilT, [])
    | otherwise = (NilT, filterTree f e ++ filterTree f d )
    where (esquerda, listaEsq) = filtro f e
          (direita, listaDir) = filtro f d

filtrandoLista :: Eq t => (t -> Bool) -> (Tree t, [Tree t]) -> [Tree t]
filtrandoLista f (x, []) = []
filtrandoLista f (x, a:as) 
 | arv == NilT = filtrandoLista f (x, as)
 | otherwise = arv : filtrandoLista f (x, as)
 where (arv, filtraHead) = filtro f a

filterTree :: Eq t => (t -> Bool) -> Tree t -> [Tree t]
filterTree _ NilT = []
filterTree f a = arv : (filtrandoLista f (arv, floresta))
 where (arv, floresta) = filtro f a
 
 ----------- EXERCICIOS DE SALA -------------
 
 -- Questao 1 --

-- exemplo: filtraListas [[1,2],[1,2,3]] 5 -- [1,2]

filtraListas :: [[Int]] -> Int -> [[Int]]
filtraListas listas filtro = filter (\x -> ((foldr (+) 0 x) < filtro )) listas

-- Questao 2 --

-- exemplo: inter [1,2,3] [2,3,4] -- [2,3]

inter :: (Eq t) => [t] -> [t] -> [t]
inter [] lista2 = []
inter (a:as) lista2 = (filter (== a) lista2) ++ (inter as lista2)

-- Questao 3 --

-- exemplo: differ [1,2,3] [3,4,5] -- [1,2]

diff :: (Eq t) => [t] -> [t] -> [t]
diff [] lista2 = []
diff (a:as) lista2 
    | ((filter (== a) lista2) == []) = a : (diff as lista2)
	| otherwise = diff as lista2
	
-- Questao 4 --

-- mapfilter :: (a->Bool) -> [[a]] -> [[a]]
