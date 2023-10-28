import ModeloDados

{-
   QUESTÃO 4  VALOR: 1,0 ponto

  Defina a função "demandaMedicamentos", cujo tipo é dado abaixo e que computa a demanda de todos os medicamentos
  por um dia a partir do receituario. O retorno é do tipo EstoqueMedicamentos e deve ser ordenado lexicograficamente
  pelo nome do medicamento.

  Dica: Observe que o receituario lista cada remédio e os horários em que ele deve ser tomado no dia.
  Assim, a demanda de cada remédio já está latente no receituario, bastando contar a quantidade de vezes que cada remédio
  é tomado.

-}

myQuickSort :: (Ord a) => [a] -> [a]
myQuickSort [] = []
myQuickSort (x:xs) = myQuickSort [y | y <- xs, y < x] ++ [x] ++ myQuickSort [y|y <- xs , y >= x]

contaDose :: [Int] -> Int
contaDose [] = 0
contaDose (x:xs) = contaDose xs + 1

demandaMedicamentos :: Receituario -> EstoqueMedicamentos
demandaMedicamentos [] = []
demandaMedicamentos ((med, (hr:hrs)): xs) = (med, contaDose(hr:hrs)):[] ++ demandaMedicamentos xs