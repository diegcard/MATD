


suma [] = 0
suma (x:xs) = x + suma xs

--1. Calcula suma de divisores
divisores n = suma [x | x <- [1..n], n `mod` x == 0]

--2. Devolver una lista con sus elemntos pares al principio y despues los impares

listaPares [] = []
listaPares (x:xs)| x `mod` 2 == 0 = x : listaPares xs  
                 | otherwise = listaPares xs 
                 
listaImpares [] = []
listaImpares (x:xs)| x `mod` 2 == 1 = x : listaImpares xs  
                   | otherwise = listaImpares xs 

listasParesImpares lista = listaPares lista ++ listaImpares lista

--3. Verficiar si la suma de los valores de una lista es impar

esImpar n = n `mod` 2 == 1

--4. Definir al funcion rep

rep n | n <= 0    = []      
      | otherwise = replicate n n
