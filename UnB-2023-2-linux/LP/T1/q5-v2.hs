-- 5)(Valor da questão: 1,5 ponto) 
-- Defina a função myZipWith que tem como parâmetros uma função binária (que tem dois parâmetros) e duas listas, 
-- retornando uma lista de valores resultantes da aplicação dessa função nos elementos correspondentes dessas 
-- listas:
l1::[Int]
l1 = [1,2,3,4,5]
l2::[Int]
l2 = [6,7,8,9,10]
l3::[Int]
l3 = [1,1,1]

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith listaSaida = []


myZipSum :: [Int] -> [Int] -> [Int]
myZipSum [] [b] = []
myZipSum [a] [] = []
myZipSum [] [] = []
myZipSum [a] [b] = [a+b]
myZipSum [a] (b:bs) = [a+b]
myZipSum (a:as) [b] = [a+b]
myZipSum (x:xs) (y:ys) = (x+y): myZipSum xs ys

mySum :: Int -> Int -> Int
mySum a b = (a + b)