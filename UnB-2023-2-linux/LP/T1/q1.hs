-- 1) (Valor da questão: 1,0 ponto) 
-- Defina uma função que retorne o maior entre quatro inteiros.
myMax :: Int -> Int -> Int
myMax a b = if a > b 
                then a
                else b

maior4 :: Int -> Int -> Int -> Int -> Int
maior4 a b c d = myMax (myMax a b) (myMax c d)