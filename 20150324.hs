menorMaior :: Int -> Int -> Int -> (Int, Int)
menorMaior x y z
    | (x >= y) && (y >= z) = (z, x) -- x >= y >= z
    | (x >= z) && (z >= y) = (y, x) -- x >= z >= y
    | (y >= x) && (x >= z) = (z, y) -- y >= x >= z
    | (y >= z) && (z >= x) = (x, y) -- y >= z >= x
    | (z >= x) && (x >= y) = (y, z) -- z >= x >= y
    | otherwise = (x, z) -- caso restante de z >= y >= x 

ordenaTripla :: (Int, Int, Int) -> (Int, Int, Int)
ordenaTripla (x, y, z)
    | (x >= y) && (y >= z) = (z, y, x) -- x >= y >= z
    | (x >= z) && (z >= y) = (y, z, x) -- x >= z >= y
    | (y >= x) && (x >= z) = (z, x, y) -- y >= x >= z
    | (y >= z) && (z >= x) = (x, z, y) -- y >= z >= x
    | (z >= x) && (x >= y) = (y, x, z) -- z >= x >= y
    | otherwise = (x, y, z) -- caso restante de z >= y >= x 

type Ponto = (Float, Float)
type Reta = (Ponto, Ponto)

xPonto :: (Ponto) -> Float
xPonto (x, y) = x

yPonto :: (Ponto) -> Float
yPonto (x, y) = y

vertical :: (Reta) -> Bool
vertical ((x1, y1), (x2, y2))
    | x1 == x2 = True
    | otherwise = False

pontoY :: Float -> Reta -> Float -- se a reta for vertical, retornara infinito
pontoY x ((x1, y1), (x2,y2)) = (((y2-y1) / (x2-x1)) * (x-x1)) + y1

type Pessoa = String
type Livro = String
type BancoDados = [(Pessoa,Livro)]

baseExemplo :: BancoDados -- livros emprestados
baseExemplo = [("Sergio", "O Senhor dos Aneis"), ("Andre", "Duna"), ("Fernando", "Johnathan Strange & Mr. Norrel"), ("Fernando", "A Game of Thrones")]

livros :: BancoDados -> Pessoa -> [Livro]
livros bd p = [livro | (pessoa,livro) <- bd, p == pessoa]

emprestimos :: BancoDados -> Livro -> [Pessoa]
emprestimos bd l = [pessoa | (pessoa,livro) <- bd, l == livro]

emprestado :: BancoDados -> Livro -> Bool
emprestado bd l
    | emprestimos bd l == [] = False
    | otherwise = True

length' :: [Livro] -> Int
length' [] = 0
length' (a:as) = (length' as) + 1

qtdEmprestimos :: BancoDados -> Pessoa -> Int
qtdEmprestimos bd p = length' (livros bd p)

emprestar :: BancoDados -> Pessoa -> Livro -> BancoDados
emprestar bd p l = (p, l) : bd

devolver :: BancoDados -> Pessoa -> Livro -> BancoDados
devolver bd p l = [ (pessoa,livro) | (pessoa,livro) <- bd, not ((l == livro) && (p == pessoa))]
