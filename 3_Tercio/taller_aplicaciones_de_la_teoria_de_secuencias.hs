-- Your code here!
n = 6
lista = [1,2,3, 1]
lista1 = [1, 2, 3, 4, 5]
lista2 = [6, 7, 8, 9, 10]
listapa = [1, 2, 3, 3, 2, 1]
listaPalist = [[3,4,4,3], [5,6,7], [5,5,5], [1,2,3,4,3,2,1],[4,5,6,5]]
listaSumaLista = [[2,2,5,5,5,5],[],[1,1,1,3,4,5,5,5,5,5, 5], [2,2,5,5],[3,1]]
listaparesimpares = [2,3,4,3,4,5,6,2,3,34,5,671]

--main = print(listaPares listaparesimpares ++ listaImpares listaparesimpares)
--main = print(listasParesImpares listaparesimpares)
--main = print(listasSuma listaSumaLista)
main = print(sufi lista)


-- 1. Dada una lista de números enteros calcular el elemento máximo.

maximo [x] = x
maximo (x:xs) = max x (maximo xs)

-- 2. Dada una lista de números enteros calcular la suma de los elementos de la lista.
suma [] = 0
suma (x:xs) = x + suma xs

-- 3. Dada una lista de números enteros calcular reverso de la lista.
reverso [] = []
reverso (x:xs) = reverso xs ++ [x]

-- 4. Dada una lista de números enteros calcular el promedio.
promedio lista = suma lista / fromIntegral (length lista)

-- 5. Dada una lista de números enteros calcular el elemento de mayor frecuencia.

-- 6. Dada una lista de enteros calcular el mínimo común múltiplo

-- 7. Dadas dos listas, de números enteros, ordenadas producir una tercera lista ordenada que tenga exactamente los elementos de la unión de las dos listas dadas.
mezcla [] ys = ys
mezcla xs [] = xs
mezcla (a:xs) (b:ys)| a <= b    = a : mezcla xs (b:ys)
                    | otherwise = b : mezcla (a:xs) ys

-- 8. Dada una lista, determinar si la lista es palindrome, ejemplo: palindrome [1,2,3,3,2,1]=True palíndrome [2,3,4]=False
esPalindromo xs = xs == reverso xs

-- 9. Hacer un programa en Haskell, tal que, dada una lista de listas, me devuelva la lista de listas que sean palindromes. Ejemplo: lisPalin ([ [3,4,4,3], [5,6,7], [5,5,5], [1,2,3,4,3,2,1],[4,5,6,5]])= [[3,4,4,3],[5,5,5],[1,2,3,4,3,2,1]]
listasPalindromas [] = []
listasPalindromas (x:xs)| esPalindromo x = x : listasPalindromas xs
                        | otherwise = listasPalindromas xs

-- 10. Construir una función en Haskell sumLista, tal que, dada una lista de listas de enteros, me devuelva una lista de enteros. Donde cada entero es la suma de los elementos de la lista correspondiente. Ejemplo sumaLista [[2,2,5,5,5,5],[],[1,1,1,3,4,5,5,5,5,5, 5], [2,2,5,5],[3,1]]=[24, 0, 40, 14, 4] que es la suma de cada lista de las listas dadas.

listasSuma [] = []
listasSuma (x:xs) = suma x : listasSuma xs
                 

-- 11. Dada una lista de lista de enteros encontrar la lista que tenga mayor suma Ejemplo: maxSum [[1,2,3], [4,2], [6,9,12],[8,87]] = [8,87]


-- 12. Definir una función que dado un entero calcule la suma de sus divisores.

divisores n = suma [x | x <- [1..n], n `mod` x == 0]

-- 13. Definir una función que dada una lista de enteros nos devuelva la lista con los mismos elementos, pero los pares al comienzo y los impares al final. Ejemplo: separa [2,3,4,3,4,5,6,2,3,34,5,671]-[2,4,4,6,2,34,3,3,5,3,5,671]

listaPares [] = []
listaPares (x:xs)| x `mod` 2 == 0 = x : listaPares xs  
                 | otherwise = listaPares xs 
                 
listaImpares [] = []
listaImpares (x:xs)| x `mod` 2 == 1 = x : listaImpares xs  
                   | otherwise = listaImpares xs 

listasParesImpares lista = listaPares lista ++ listaImpares lista

-- 14. Definir una función rep que dado un entero positivo n calcule una lista con el número repetidas n veces. Por ejemplo: rep 2=[[2],[2]], rep 1=[[1]], rep 5 = [[5],[5],[5],[5],[5]]

rep n | n <= 0    = []      
      | otherwise = replicate n [n]
      
-- 15. Dada una lista: calcular la lista de todos sus sufijos.
sufi [] = []
sufi (x:xs) = xs:(sufi xs) 
