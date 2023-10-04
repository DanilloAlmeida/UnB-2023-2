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

