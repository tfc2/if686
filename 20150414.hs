data Grafos t = Nil | Grafo [(t, [(t,Int)])] deriving (Show, Eq) -- grafico representado por vertice e lista de (adjacencente, peso)

-- exemplo:
g = Grafo ([(1,[(1,1),(2,1)]), (2,[(1,2)])])

-- para o map e o fold, consideramos que as funções são aplicadas nos vertices

{-
mapGraph :: (Grafos t -> Grafos t) -> Grafos t -> Grafos t
mapGraph f Nil = Nil
mapGraph f (Grafo ((vertice, adjacencias):as)) =  Grafo ( (f vertice, adjacencias) : mapGraph f (Grafo (as)) )
-}
-- definir funcao add (vertice == vertice + 1) para testar

foldGraph :: (t -> [t] -> [t]) -> t -> Grafos t -> [t]
foldGraph f t Nil = []
foldGraph f s (Grafo ((vertice, adjacencias):as)) = f (vertice) (foldGraph f s (Grafo (as)))
-- definir funcao bool (vertice == elemento) para testar
