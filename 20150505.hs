---- Trabalho 11 ----

import Control.Monad
import Data.Char

-- Questao 1 --

type Table = [(Int, Int)] -- Primeiro int = chave, segundo int = valor

getElem :: Table -> Int -> Maybe [Int]
getElem [] _ = Nothing
getElem ((a, as):bs) n
 | a == n = Just [as]
 | otherwise = getElem bs n

hasKey :: Table -> Int -> Bool
hasKey [] _ = False
hasKey ((a, as):bs) n
 | a == n = True
 | otherwise = hasKey bs n

putElem :: Table -> (Int, Int) -> Maybe Table
putElem as (x, y) 
 | hasKey as x = Just as -- Nao adiciona se ja existir a chave
 | otherwise = Just (as ++ [(x,y)])

removeElem :: Table -> Int -> Maybe Table -- int = chave
removeElem [] _ = Nothing
removeElem xs x = Just [(a, as) | (a, as) <- xs, (a /= x)]

exemplo = do
	{
		a <- putElem [(1, 3),(2, 4), (5, 7)] (6,8); -- incluindo elemento 1
		b <- putElem a (9,11); -- incluindo elemento 2
		c <- removeElem b 1; -- remoção
		d <- putElem c (10,12); -- incluindo elemento 3
		e <- putElem d (13,15); -- incluindo elemento 4
		f <- removeElem e 2; -- remoção
		g <- putElem f (14,16); -- incluindo elemento 5
		h <- putElem g (17,19); -- incluindo elemento 6
		i <- removeElem h 5; -- remoção
		j <- putElem i (18,20); -- incluindo elemento 7
		k <- putElem j (21,23); -- incluindo elemento 8
		l <- removeElem k 6; -- remoção
		m <- putElem l (22,24); -- incluindo elemento 9
		n <- putElem m (25,27); -- incluindo elemento 10
		removeElem n 9; -- remoção
	}

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