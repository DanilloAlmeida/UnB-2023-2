-- 5)(Valor da questão: 1,5 ponto) 
-- Defina a função myZipWith que tem como parâmetros uma função binária (que tem dois parâmetros) e duas listas, 
-- retornando uma lista de valores resultantes da aplicação dessa função nos elementos correspondentes dessas 
-- listas:
l1 = [1,2,3]
l2 = ["a","b","c"]
myZip :: z -> t -> (z,t)
myZip a b = (a, b)


myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith func [] [] = []
myZipWith func [] (x:xs) = []
myZipWith func (x:xs) [] = []
myZipWith func (x:xs) (y:ys) = (func x y ): myZipWith func xs ys