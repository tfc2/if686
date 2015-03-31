----------- questao 1 -------------

{- 
O polimorfismo de sobrecarga ocorre quando temos funções de mesmo nome, porém distintas para cada tipo. Em Java, isso é feito através de criação de 
métodos de mesmo nome, porém de assinaturas diferentes, seja pelo número ou tipo de argumentos. Já em Haskell, isso é feito através da utilização 
de classes de tipos, que permitem definir diferentes tipos para uma função ou operação. Dessa forma, se por um lado em Haskell fica mais simples 
definirmos uma função que utiliza classes de tipos para suportar a sobrecarga, ao invés de mais de um método de diferentes argumentos; por outro 
lado, acaba sendo mais custoso por ser necessário realizar o processo de unificação, que é a correspondência entre os argumentos e parâmetros passados.
Referências
CASTOR, Fernando. Polimorfismo. Disponível em: <https://docs.google.com/a/cin.ufpe.br/viewer?a=v&pid=sites&srcid=Y2luLnVmcGUuYnJ8aWY2ODZ8Z3g6NDYwNmI3ZGMxNDExMjFlOQ>. Acesso em: 28 mar. 2015.
MEDEIROS, Higor. Uso de Polimorfismo em Java. Disponível em: <http://www.devmedia.com.br/uso-de-polimorfismo-em-java/26140>. Acesso em: 28 mar. 2015.
OLIVEIRA, Guilherme Gomes Neves de; PETRI, Renzo Augusto Lapelligrini. Haskell: Seminário de Linguagens de Programação. Disponível em: <http://pt.slideshare.net/renzopetri/seminario-haskell>. Acesso em: 28 mar. 2015.
-}

----------- questao 2 -------------

quebraLook :: (Show t, Num t) => String -> t -> String
quebraLook (a:[]) n = (show n) ++ [a]
quebraLook (a:as) n
 | a == (head as) = quebraLook as (1+n)
 | otherwise = (show n) ++ [a] ++ quebraLook as 1

repeat' :: (Eq t, Num t) => String -> t -> String
repeat' s 1 = "1"
repeat' s 2 = quebraLook s 1
repeat' s n = repeat' (quebraLook s 1) (n-1)

lookAndSay :: (Eq t, Num t) => t -> String
lookAndSay n = repeat' "1" n

----------- questao 3 -------------

{-
Defina um tipo de dados que representa um grafo não necessariamente conexo, onde cada nó tem um rótulo:
type Grafo = [(vertice, [adjacencias])], ou seja, [(t, [t])]
-}

{-
Exemplo de grafo: [(1,[2,3]), (2,[1,3,4]), (3,[1,2,4]), (4,[2,3]), (5,[])]
-}

listaVertices :: (Eq t) => [(t,[t])] -> [(t,Bool)] -- constroi a lista de vertices com a flag booleana "visitado" = False, usando o grafo como entrada
listaVertices [] = []
listaVertices ((x,y):as) = [(x,False)] ++ (listaVertices as)

marcaVertices :: (Eq t) => [(t,Bool)] -> t -> Bool -> [(t,Bool)] -- altera "visitado" usando como entrada a lista de vertices, o vertice e True or False
marcaVertices [] vertice visitado = []
marcaVertices ((x,y):as) vertice visitado
    | x == vertice = [(x,visitado)] ++ (marcaVertices as vertice visitado)
    | otherwise = [(x,y)] ++ (marcaVertices as vertice visitado)

adjacentes :: (Eq t) => [(t,[t])] -> t -> [t] -- retorna a lista de vertices adjacents a um vertice t usando o grafo como entrada
adjacentes [] vertice = []
adjacentes ((x,y):as) vertice
    | x == vertice = y
    | otherwise = adjacentes as vertice

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

criaCaminho :: (Eq t) => [t] -> [(t,t)] -- pega a pilha e remonta o caminho do vertice inicial ao desejado
criaCaminho [] = []
criaCaminho (a:as)
    | as == [] =  [] -- quando é o último elemento não se cria um caminho dele até o vazio
    | otherwise = (criaCaminho as)++[((head as), a)]

busca :: (Eq t) => [(t,[t])] -> [(t,Bool)] -> [t] -> t -> t -> [(t,t)] -- funcao de busca em profundidade
busca grafo vertices [] inicio fim = [] -- caso base de a pilha estar vazia (terem acabado os vertices adjacentes não visitados)
busca grafo vertices pilha inicio fim
    | (proximoAdjacente (adjacentes grafo inicio) vertices) == [] = busca grafo vertices (tail pilha) inicio fim -- não há adjacente válido, volta a pilha
    | head (proximoAdjacente (adjacentes grafo inicio) vertices) == fim = criaCaminho (fim:pilha) -- chegou no vertice pretendido, retorna o caminho
    | otherwise = busca grafo (marcaVertices vertices inicio True) ((proximoAdjacente (adjacentes grafo inicio) vertices)++pilha) (head (proximoAdjacente (adjacentes grafo inicio) vertices)) fim 
    -- marca o proximo vertice adjacente como visitado, o coloca na pilha e o considera como inicio

search :: (Eq t) => [(t,[t])] -> t -> t -> [(t,t)] -- funcao inicial que marca vertices como não lidos, define o incial como visitado e o coloca na fila
search grafo inicio fim = busca grafo (marcaVertices (listaVertices grafo) inicio True) [inicio] inicio fim

----------- questao 4 -------------

-- Ex: [[9,4,5,0,8],[10,3,2,1,7],[9,1,6,3,15], [0,3,8,10,1],[1,16,9,12,7]]	

-- Funcoes para ajudar a encontrar quais elementos fazem parte da regiao que sera calculada a mediana --

lista :: [Int] -> Int -> [Int] -- Retorna uma lista de n primeiros numeros
lista [] _ = []						
lista (a:as) n | n > 0 = a:lista as (n-1)
           | otherwise = lista as n

listaIntervalo :: [Int] -> Int -> Int -> [Int] -- Retorna uma lista em um certo intervalo da matriz
listaIntervalo (a:as) n tam = lista (drop (n - (tam `div` 2) - 1) (a:as)) tam

listaLinhas :: [[Int]] -> Int -> Int -> Int -> [[Int]] -- Coloca os elementos que influenciam na mediana em uma lista dentro da linha
listaLinhas [] _ tam j  = []
listaLinhas (a:as) n tam j | n > 0 = listaIntervalo a j tam:listaLinhas as (n-1) tam j
				           | otherwise = listaLinhas as (n) tam j

linhas ::  [[Int]] -> Int -> Int -> Int -> [[Int]]  -- Retorna as linhas com listas de elementos que sao influenciadas pelo tamanho do filtro
linhas (a:as) n tam j = listaLinhas (drop (n - (tam `div` 2) - 1) (a:as)) tam tam j

-- Fim dessas funcoes de apoio --

regiao :: [[Int]] -> Int -> Int -> Int -> [[Int]] -- Retorna os elementos da regiao que irao influenciar na mediana
regiao [] _ _ _ = []
regiao (a:as) i j n
    | (i > n `div` 2 && j > n `div` 2 && i < length (a:as) - n `div` 2 + 1 && j < length (a:as) - n `div` 2 + 1) = linhas (a:as) i n j
	| otherwise = []
-- Calculos retirados do pseudo-codigo do algoritmo do filtro 2D de mediana - http://en.wikipedia.org/wiki/Median_filter

concatena :: [[Int]]  -> [Int] -- Concatena os elementos que fazem parte da regiao para ser feito o calculo da mediana linha a linha
concatena []  = []
concatena (a:as) = a++concatena as

quickSort :: [Int] -> [Int] -- Quicksort para ordenar os numeros e poder pegar a mediana
quickSort [] = []
quickSort (a:as) = quickSort [b | b <- as, b < a] ++ [a] ++ quickSort [b | b <- as, b >= a]

mediana :: [Int] -> Int -- Calcula o valor da mediana dado um conjunto de elementos ordenadamente listados que definem a mediana
mediana [] = 0
mediana (a:as) = head (drop (length (a:as) `div`2) (a:as))

novaMatriz :: [[Int]] -> Int -> Int -> [Int] -- Gera a nova matriz a partir da origianl, do elemento que deve ser recalculado e de n (dimensao do filtro)
novaMatriz [] _ _ = []
novaMatriz (a:as) elemento n =[mediana (quickSort (concatena (regiao (a:as) elemento e n))) | e <- [1..length(a:as)]]

filtroMediana :: [[Int]] -> Int -> [[Int]] -- Funcao principal que recebe a matriz e n (dimensao do filtro)
filtroMediana [] _ = []
filtroMediana (a:as) n = [novaMatriz (a:as) elemento n | elemento <- [1..length(a:as)]]
