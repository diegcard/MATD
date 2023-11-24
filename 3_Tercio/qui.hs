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

--
copiar :: [Int] -> [Int] -> [Int]
copiar [] _ = []  
copiar (x:xs) (n:ns) = replicate n x ++ copiar xs ns

ejemplo1 = copiar [4,4,3,0,31] [3,0,3,7,2] 
ejemplo2 = copiar [3,4,5] [1,3,2]   

main = print(ejemplo2)
--
import Data.List (nub)

impLs :: (Eq a) => [a] -> [a]
impLs lista = filter (\x -> odd $ contarApariciones x lista) $ nub lista

contarApariciones :: (Eq a) => a -> [a] -> Int
contarApariciones x = length . filter (== x)

-- Ejemplos
ejemplo1 = impLs [3,2,4,4,6,7,7,7,0,2] -- Resultado: [3,6,0,7]
ejemplo2 = impLs [3,3,2,4,4,6,7,7,2,7,0,2,4,4,4,6] -- Resultado: [2,4,7,0]

main = print(ejemplo1)