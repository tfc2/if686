--- Trabalho 11 ---

import Control.Monad
import Data.Char

-- Questao 1 --

data Table = Somente [(Int, Int)] | Nada -- Primeiro int = chave, segundo int = valor

instance Monad Table where
 (>>=) (Somente x) f = f x
 (>>=) Nada _ = Nada
 return Somente x = Somente x

getElem :: Table -> Int -> Table
getElem Nothing _ = Nada
getElem ((a, as):bs) n
 | a == n = Somente [as]
 | otherwise = Somente getElem bs n

hasKey :: Table -> Int -> Bool
hasKey Nothing _ = False
hasKey ((a, as):bs) n
 | a == n = True
 | otherwise = hasKey bs n

putElem :: Table -> (Int, Int) -> Table
putElem as (x, y) 
 | hasKey as x = Somente as -- Nao adiciona se ja existir a chave
 | otherwise = Somente (as ++ [(x,y)])

removeElem :: Table -> Int -> Table -- int = chave
removeElem Nothing _ = Nada
removeElem xs x = Somente [(a, as) | (a, as) <- xs, (a /= x)]

-- Questao 2 --

avaliarAux :: Maybe String -> Bool
avaliarAux (Just []) = True
avaliarAux (Just (a:as))
 | (a == ' ') || (isLetter a)  = avaliarAux (Just as)
 | otherwise = False

avaliar :: Maybe String -> Maybe String
avaliar str
 | avaliarAux str = str
 | otherwise = Nothing

maiuscula :: Maybe String -> Maybe String
maiuscula Nothing = Nothing
maiuscula (Just str) = Just [toUpper s | s <- str]

quebraAux :: String -> String -> [String]
quebraAux [] ac = [ac]
quebraAux (a:as) ac
 | a == ' ' = ac:(quebraAux as [])
 | otherwise = (quebraAux as (ac++[a]))

quebra :: Maybe String -> [String]
quebra Nothing = [[]]
quebra (Just str) = quebraAux str []

main :: IO()
main = forever $ do  
    putStr "Digite uma cadeia de caracteres: "  
    str <- getLine
    str2 <- return (avaliar (Just str))
    str3 <- return (maiuscula str2)
    str4 <- return (quebra str3)
    mapM_ putStrLn str4