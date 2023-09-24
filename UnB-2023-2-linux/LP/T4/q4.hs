lista1 = ["casa", "quadrado", "quadrado", "bar", "casa", "caso"]


-- 4) (Valor da questão: 2,0 pontos) 
-- defina uma função que recebe uma lista de strings como entrada e computa uma lista de pares 
-- de (String, Int) representando o histograma de seus elementos:
zipContaString :: String -> Int -> (String, Int)
zipContaString str qtd = (str, qtd)
 
contaString :: [String] -> String -> Int
contaString [] s = 0
contaString (a:as) s    | a == s = (contaString as s) + 1 
                        | otherwise = contaString as s

retiraUmElemento :: String -> [String] -> [String]
retiraUmElemento str (a:as)     | str == a = as
                                | otherwise = a: retiraUmElemento str as

retiraTodosElementos :: String -> [String] -> [String]
retiraTodosElementos str [] = []
retiraTodosElementos str (a:as)     | str == a = retiraTodosElementos a (retiraUmElemento str (a:as))
                                    | otherwise = a : retiraTodosElementos str as                                

histograma :: [String] -> [(String, Int)]
histograma [] = []
histograma (a:as) = (zipContaString a (contaString (a:as) a)): histograma (retiraTodosElementos a (a:as))