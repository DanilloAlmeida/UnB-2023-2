-- 3) (Valor da questão: 1,0 ponto) 
-- defina uma função que retorna um booleano indicando se uma lista de inteiros é decrescente ou não:
takeFirst :: [Int] -> Int
takeFirst (x:xs) = x

isDecrescente :: [Int] -> Bool
isDecrescente [x] = True
isDecrescente [] = True
isDecrescente (a:as) | a <= takeFirst as = False
                     | otherwise = isDecrescente as