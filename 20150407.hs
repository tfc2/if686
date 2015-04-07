----- Questao 1 -----

-- Ex. 1: area (Circle 4.9 :: Shape) -- resultado = 30.78761
-- Ex. 2: area (Rectangle 4.2 2.0 :: Shape) -- resultado = 8.4

data Shape = Circle Float | Rectangle Float Float

isRound :: Shape -> Bool
isRound (Circle _) = True
isRound (Rectangle _ _) = False

area :: Shape -> Float
area shape
    | isRound shape = areaCircle shape
	| otherwise = areaRectangle shape
	
areaCircle :: Shape -> Float
areaCircle (Circle raio) = 2 * pi * raio

areaRectangle :: Shape -> Float
areaRectangle (Rectangle altura largura) = altura * largura

----- Questao 2 -----  

-- Ex. 1: fimSemana Sabado -- resultado = True
-- Ex. 2: fimSemana (Segunda 4 ["Compiladores", "PLC"]) -- resultado = False
-- Ex. 3: aulaPLC (Segunda 4 ["Compiladores", "PLC"]) -- resultado = True
-- Ex. 4: aulaPLC Sabado -- resultado = False

data Dia = Segunda Int [String] | Terca Int [String] | Quarta Int [String] | Quinta Int [String] | Sexta Int [String] | Sabado | Domingo

fimSemana :: Dia -> Bool
fimSemana Sabado = True
fimSemana Domingo = True
fimSemana _ = False

aulaPLC :: Dia -> Bool
aulaPLC Sabado = False
aulaPLC Domingo = False
aulaPLC (Segunda horaAula disciplinas) = procuraPLC disciplinas
aulaPLC (Terca horaAula disciplinas) = procuraPLC disciplinas
aulaPLC (Quarta horaAula disciplinas) = procuraPLC disciplinas
aulaPLC (Quinta horaAula disciplinas) = procuraPLC disciplinas
aulaPLC (Sexta horaAula disciplinas) = procuraPLC disciplinas
	
procuraPLC :: [String] -> Bool
procuraPLC [] = False
procuraPLC (a:as)
    | a ==  "PLC" = True
	| otherwise = procuraPLC as
	
----- Questao 3 -----

-- Uma opcao seria usar deriving (Show, Eq)

data Tree t = NilT | Node t (Tree t) (Tree t) 

showTree :: Show t => Tree t -> String
showTree NilT = []
showTree (Node no (esquerda) (direita)) = (show no) ++ showTree(esquerda) ++ showTree(direita)

eqTree :: Eq t => Tree t -> Tree t -> Bool
eqTree NilT NilT = True
eqTree NilT _ = False
eqTree _ NilT = False
eqTree (Node no1 (esquerda1) (direita1)) (Node no2 (esquerda2) (direita2))
    | (no1 == no2) = (eqTree esquerda1 esquerda2) && (eqTree direita1 direita2)

----- Questao 4 -----

data Expr = Lit Int | Add Expr Expr | Sub Expr Expr

showExpr :: Expr -> String
showExpr (Lit n) = show n
showExpr (Add e1 e2) = (showExpr e1) ++ "+" ++ (showExpr e2)
showExpr (Sub e1 e2) = (showExpr e1) ++ "-" ++ (showExpr e2)