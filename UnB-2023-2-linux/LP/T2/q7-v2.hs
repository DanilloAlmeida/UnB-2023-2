import ModeloDados
{-
   QUESTÃO 7  VALOR: 1,0 ponto

  Defina a função "geraPlanoReceituario", cujo tipo é dado abaixo e que, a partir de um receituario válido,
  retorne um plano de medicamento válido.

  Dica: enquanto o receituário lista os horários que cada remédio deve ser tomado, o plano de medicamentos  é uma
  disposição ordenada por horário de todos os remédios que devem ser tomados pelo paciente em um certo horário.

-}
isElem :: Horario -> [Horario] -> Bool
isElem _ [] = False
isElem hora (hr:hrs) = if hora == hr then True
                        else isElem hora hrs

montaParMedHr :: (Medicamento, [Horario]) -> [(Medicamento, Horario)]
montaParMedHr (_, []) = []
montaParMedHr (med, hr:hrs) = (med, hr):montaParMedHr (med, hrs)

montaParMedHrRec :: Receituario -> [(Medicamento, Horario)]
montaParMedHrRec [] = []
montaParMedHrRec ((med, hr:hrs): xs) = montaParMedHr (med, hr:hrs) ++ montaParMedHrRec xs

montaListaHrDeListaDePar :: [(Medicamento, Horario)] -> [Horario] -> [Horario]
montaListaHrDeListaDePar [] aux = aux
montaListaHrDeListaDePar ((med, hr):xs) aux =    if isElem hr aux
                                                            then montaListaHrDeListaDePar xs aux
                                                            else montaListaHrDeListaDePar xs (hr:aux)

montaPlanoVazio :: [Horario] -> PlanoMedicamento-> PlanoMedicamento
montaPlanoVazio [] aux = aux
montaPlanoVazio (x:xs) aux = montaPlanoVazio xs ((x,[""]):aux)

addParMedHrNoPlan :: (Medicamento, Horario) -> PlanoMedicamento -> PlanoMedicamento -> PlanoMedicamento
addParMedHrNoPlan (_,_) [] aux = aux
addParMedHrNoPlan (med, hr) ((hrEnt, medEnt:medsEnt):xs) aux = if hr == hrEnt
                                                        then addParMedHrNoPlan (med, hr) xs (aux ++ [(hr, med:(medEnt:medsEnt))])
                                                        else addParMedHrNoPlan (med, hr) xs (aux ++ [(hrEnt, medEnt:medsEnt)])

addListParMedHrNoPlan :: [(Medicamento, Horario)] -> PlanoMedicamento -> PlanoMedicamento
addListParMedHrNoPlan [] plan = plan
addListParMedHrNoPlan ((med,hr):xs) plan = addListParMedHrNoPlan xs (addParMedHrNoPlan (med,hr) plan [])

geraPlanoReceituario :: Receituario -> PlanoMedicamento
geraPlanoReceituario receit = ordenaMedPlano (limpaVazio (addListParMedHrNoPlan (montaParMedHrRec receit) (montaPlanoVazio (myQuickSortAsc (montaListaHrDeListaDePar (montaParMedHrRec receit) [])) [])) []) []

myQuickSortAsc :: (Ord a) => [a] -> [a]
myQuickSortAsc [] = []
myQuickSortAsc (x:xs) = myQuickSortAsc [y | y <- xs, y < x] ++ [x] ++ myQuickSortAsc [y|y <- xs , y >= x]

-- myQuickSortAsc' :: (Ord a) => [a] -> [a]
-- myQuickSortAsc' [] = []
-- myQuickSortAsc' ((hr,[med]):xs) = myQuickSortAsc [y | y <- xs, y < x] ++ [x] ++ myQuickSortAsc [y|y <- xs , y >= x]

myQuickSortDesc :: (Ord a) => [a] -> [a]
myQuickSortDesc [] = []
myQuickSortDesc (x:xs) = myQuickSortDesc [y | y <- xs, y >= x] ++ [x] ++ myQuickSortDesc [y|y <- xs , y < x]

limpaVazio :: PlanoMedicamento -> PlanoMedicamento -> PlanoMedicamento
limpaVazio [] aux = aux
limpaVazio ((hr, med:meds):xs) aux = limpaVazio xs ((hr, apagaUltimo (med:meds)):aux)

apagaUltimo :: [a] -> [a]
apagaUltimo [x]=[]
apagaUltimo (y:ys) = y:apagaUltimo ys

ordenaMedPlano :: PlanoMedicamento -> PlanoMedicamento -> PlanoMedicamento
ordenaMedPlano [] aux = aux
ordenaMedPlano ((hr, med):xs) aux = ordenaMedPlano xs (aux ++ [(hr, ordenaMed med)])

ordenaMed :: (Ord a) => [a] -> [a]
ordenaMed [] = []
ordenaMed (x:xs) = ordenaMed [y | y <- xs, y < x] ++ [x] ++ ordenaMed [y|y <- xs , y >= x]