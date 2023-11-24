--1
telescópica :: Int -> [Int]
telescópica 0 = []
telescópica n = telescópicaAux 1 n []

telescópicaAux :: Int -> Int -> [Int] -> [Int]
telescópicaAux _ 0 acc = acc
telescópicaAux k n acc = telescópicaAux (k + 1) (n - 1) (acc ++ replicate k k)

main :: IO ()
main = do
  let resultado = telescópica 4
  putStrLn $ show resultado

--2


--3

ordenarBurbuja :: Ord a => [a] -> [a]
ordenarBurbuja lista = burbujaIter lista (length lista)

burbujaIter :: Ord a => [a] -> Int -> [a]
burbujaIter lista 0 = lista
burbujaIter lista n = burbujaIter (burbujaPasada lista) (n - 1)

burbujaPasada :: Ord a => [a] -> [a]
burbujaPasada [] = []
burbujaPasada [x] = [x]
burbujaPasada (x:y:resto)
  | x > y     = y : burbujaPasada (x:resto)
  | otherwise = x : burbujaPasada (y:resto)

main :: IO ()
main = do
  let listaDesordenada = [4, 2, 7, 1, 9, 5]
  let listaOrdenada = ordenarBurbuja listaDesordenada
  putStrLn $ "Lista Desordenada: " ++ show listaDesordenada
  putStrLn $ "Lista Ordenada: " ++ show listaOrdenada

--4

segmento :: Int -> Int -> [a] -> [a]
segmento p q lista = take (q - p + 1) (drop (p - 1) lista)

main :: IO ()
main = do
  let ejemplo1 = segmento 2 8 [4,3,4,5,6,7,2,5,234242]
  let ejemplo2 = segmento 12 43 [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,45]

  putStrLn $ "Ejemplo 1: " ++ show ejemplo1
  putStrLn $ "Ejemplo 2: " ++ show ejemplo2

--5

listaPares [] = []
listaPares (x:xs)| x `mod` 2 == 0 = x : listaPares xs  
                 | otherwise = listaPares xs 
                 
listaImpares [] = []
listaImpares (x:xs)| x `mod` 2 == 1 = x : listaImpares xs  
                   | otherwise = listaImpares xs 

separa  lista = listaPares lista ++ listaImpares lista

main :: IO ()
main = do
  let ejemplo1 = separa [4,3,4,5,6,7,2,5,234242]
  putStrLn $ "Ejemplo 1: " ++ show ejemplo1

--6

prefijos :: [a] -> [[a]]
prefijos lista = [take n lista | n <- [0..length lista]]

main :: IO ()
main = do
  let ejemplo = prefijos [1, 2, 3, 4]
  putStrLn $ "Ejemplo: " ++ show ejemplo

--7

lisCaPaUn :: [[Int]] -> [[Int]]
lisCaPaUn listas = filter (\l -> even (countUnos l)) listas

countUnos :: [Int] -> Int
countUnos lista = length (filter (== 1) lista)

main :: IO ()
main = do
  let ejemplo = lisCaPaUn [[3,4,4,1,3,1], [5,6,1,1,7,1], [5,5,5], [1,2,1,3,4,3,1,2,1], [4,5,6,5,1]]
  putStrLn $ "Ejemplo: " ++ show ejemplo


--8

maxmin :: Ord a => [a] -> [a]
maxmin lista = [x | (i, x) <- zip [0..] lista, todosMenoresAntes i lista && todosMenoresDespues i lista]

todosMenoresAntes :: Ord a => Int -> [a] -> Bool
todosMenoresAntes i lista = all (< lista !! i) (take i lista)

todosMenoresDespues :: Ord a => Int -> [a] -> Bool
todosMenoresDespues i lista = all (< lista !! i) (drop (i + 1) lista)

main :: IO ()
main = do
  let ejemplo1 = maxmin [3,3,3,5,1,1,1]
  let ejemplo2 = maxmin [3,4,6,2,65,12,3,14]
  let ejemplo3 = maxmin [5,5,5,5,5,83,9]
  let ejemplo4 = maxmin [2,3,4]

  putStrLn $ "Ejemplo 1: " ++ show ejemplo1
  putStrLn $ "Ejemplo 2: " ++ show ejemplo2
  putStrLn $ "Ejemplo 3: " ++ show ejemplo3
  putStrLn $ "Ejemplo 4: " ++ show ejemplo4


--9



--10

import Data.List (nub)

listSegCost :: [[Int]] -> [[Int]]
listSegCost lista = map nub lista

main :: IO ()
main = do
  let ejemplo1 = listSegCost [[4,4,4],[3],[4],[5],[6,6,1,6,6],[2],[3],[42,42]]
  let ejemplo2 = listSegCost [[3,4,2,32,2,2,4],[1,5,6,5],[6],[7],[8,3,3],[1,1,1,34,1,1],[5,5],[6],[7],[8],[1,1,1]]
  let ejemplo3 = listSegCost [[5,5,5,5,5,5]]
  let ejemplo4 = listSegCost [[1],[2],[3],[4]]

  putStrLn $ "Ejemplo 1: " ++ show ejemplo1
  putStrLn $ "Ejemplo 2: " ++ show ejemplo2
  putStrLn $ "Ejemplo 3: " ++ show ejemplo3
  putStrLn $ "Ejemplo 4: " ++ show ejemplo4

