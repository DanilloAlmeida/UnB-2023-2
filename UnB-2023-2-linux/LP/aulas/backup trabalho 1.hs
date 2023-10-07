{-
 NOME: 
 MATRICULA:
 
** Intruções: **
* Favor preencher nome e matricula acima
* NÃO importe nenhuma biblioteca EXCETO se na descrição do exercício estiver explícito.
-}

module Root.Exercicios.Exercicios where

-- 1) (Valor da questão: 1,0 ponto) 
-- Defina uma função que retorne o maior entre quatro inteiros.
myMax :: Int -> Int -> Int
myMax a b = if a > b 
                then a
                else b

maior4 :: Int -> Int -> Int -> Int -> Int
maior4 a b c d = myMax (myMax a b) (myMax c d)

-- 2) (Valor da questão: 1,0 ponto) 
-- Defina uma função que receba uma nota e retorne a menção do aluno.
-- Não se preocupe com a validação do input. A nota sempre será um Número entre 0.0 (inclusive) e 10.0 (inclusive).
-- Considere a seguinte tabela para tradução da menção:
-- De 9 a 10 -> "SS"
-- De 7 a 8.9 -> "MS"
-- De 5 a 6.9 -> "MM"
-- De 3 a 4.9 -> "MI"
-- De 0.1 a 2.9 -> "II"
-- De 0 -> "SR"
converterNotaParaMencao :: Float -> String
converterNotaParaMencao nota 
                        -- | nota == 0 = "SR"
                        | nota >= 9 && nota <=10 = "SS"
                        | nota >= 7 && nota <=8.9 = "MS"
                        | nota >= 5 && nota <=6.9 = "MM"
                        | nota >= 3 && nota <=4.9 = "MI"
                        | nota >= 0.1 && nota <=2.9 = "II"
                        | otherwise = "SR"

-- 3) (Valor da questão: 1,0 ponto) 
-- defina uma função que retorna um booleano indicando se uma lista de inteiros é decrescente ou não:
takeFirst :: [Int] -> Int
takeFirst (x:xs) = x

isDecrescente :: [Int] -> Bool
isDecrescente [x] = True
isDecrescente [] = True
isDecrescente (a:as) | a <= takeFirst as = False
                     | otherwise = isDecrescente as

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


-- 5)(Valor da questão: 1,5 ponto) 
-- Defina a função myZipWith que tem como parâmetros uma função binária (que tem dois parâmetros) e duas listas, 
-- retornando uma lista de valores resultantes da aplicação dessa função nos elementos correspondentes dessas 
-- listas:
myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith = undefined 


-- 6) (Valor da questão: 2,0 ponto) 
-- Resolva em Haskell o seguinte problema: a partir de duas notas das provas de cada aluno,
-- determinar a lista dos alunos aprovados, com suas respectivas médias. O resultado deve estar
-- ordenado crescentemente pela média aritmética das notas. A aprovação ocorre se, e somente se, tal
-- média é maior ou igual a cinco.
-- OBSERVAÇÃO: especificamente para este exercício, você pode importar as funções de ordenaçao de listas (como 'sort' ou 'sortBy') se achar necessário.
dbNomeNotas1 :: [(String, Float, Float)]
dbNomeNotas1 = [("Alex", 4.5, 4.5),
                ("Danillo", 4.5, 5.5), 
                ("Daniel", 7, 8), 
                ("Demetrio", 7, 8), 
                ("Felipe", 10.0, 10.0), 
                ("Rodolfo", 0.0, 0.0)]

calculaMedia :: (String,Float,Float) -> (String,Float)
calculaMedia (str, n1, n2) = (str, ((n1+n2)/2))

getMedia :: (String, Float) -> Float
getMedia (str, media) = media

listaDeMedias :: [(String,Float,Float)] -> [(String,Float)]
listaDeMedias [] = []
listaDeMedias (x:xs) = (calculaMedia x) : listaDeMedias xs

listaAprovados :: [(String,Float)] -> [(String,Float)]
listaAprovados [] = [] 
listaAprovados (x:xs)   | getMedia x >= 5 = x : listaAprovados xs
                        | otherwise = listaAprovados xs

--TRATAMENDO DE ORDENTAÇÃO
mySortGetMedia :: (String,Float) -> Float
mySortGetMedia (str, flt) = flt

mySortGetElement :: [(String,Float)] -> (String,Float)
mySortGetElement [x] = x
mySortGetElement (x:xs) = x


mySortGetMenor :: [(String,Float)] -> (String,Float)
mySortGetMenor [x] = x
mySortGetMenor (x:xs) | (mySortGetMedia x) < (mySortGetMedia (mySortGetMenor xs)) = x
                | otherwise = mySortGetMenor xs

mySortRemoveMenor :: [(String,Float)] -> [(String,Float)]
mySortRemoveMenor [x] = []
mySortRemoveMenor (x:xs)  | x == mySortGetMenor (x:xs) = xs
                    | otherwise = x: mySortRemoveMenor xs

mySortAux :: [(String,Float)] -> [(String,Float)] -> [(String,Float)]
mySortAux listaOrdenada [] = listaOrdenada
mySortAux listaOdenada (x:xs) = mySortAux ( listaOdenada ++ [mySortGetMenor (x:xs)]) (mySortRemoveMenor (x:xs))                   

mySort :: [(String,Float)] -> [(String,Float)]
mySort [] = []
mySort (x:xs) = mySortAux [] (x:xs)                       

aprovadosOrdemDeMedia :: [(String,Float,Float)] -> [(String,Float)]
aprovadosOrdemDeMedia lista = mySort (listaAprovados (listaDeMedias lista))

-- 7) (Valor da questão: 1,5 ponto, sendo 0.5 ponto para cada letra) 
-- Considere a representação de matrizes como lista de listas em que cada elemento da lista é uma lista 
-- que representa uma linha da matriz. Com base nisso, determine as seguintes funções:
--  a) some duas matrizes
--  b) compute a transposta de duas matrizes 
--  c) compute a multiplicação de duas matrizes
-- OBSERVAÇÃO: considere que os inputs são válidos (ou seja, as matrizes são válidas e as suas dimensões são compatíveis para soma e multiplicação)
somaMatricial :: Num u => [[u]] -> [[u]] -> [[u]]
somaMatricial = undefined 

matrizTransposta :: Num u => [[u]] -> [[u]]
matrizTransposta = undefined 

multiplicacaoMatricial :: Num u => [[u]] -> [[u]] -> [[u]]
multiplicacaoMatricial = undefined 
