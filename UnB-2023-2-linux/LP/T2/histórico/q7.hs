import ModeloDados
{-
   QUESTÃO 7  VALOR: 1,0 ponto

  Defina a função "geraPlanoReceituario", cujo tipo é dado abaixo e que, a partir de um receituario válido,
  retorne um plano de medicamento válido.

  Dica: enquanto o receituário lista os horários que cada remédio deve ser tomado, o plano de medicamentos  é uma
  disposição ordenada por horário de todos os remédios que devem ser tomados pelo paciente em um certo horário.

-}
-- extMed :: [(Medicamento, [Horario])] -> [Medicamento]
-- extMed [] = []
-- extMed ((med, hr):xs) = med:extMed xs

-- extHrs :: (Medicamento, [Horario]) -> [Horario]
-- extHrs (_ , []) = []
-- extHrs (med, hr:hrs) = hr : extHrs (med, hrs)

-- receituario4 :: Receituario
-- receituario4 = [(med4, [8, 9, 10, 17])]

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
                                                        then addParMedHrNoPlan (med, hr) xs ((hr, med:(medEnt:medsEnt)):aux)
                                                        else addParMedHrNoPlan (med, hr) xs ((hrEnt, medEnt:medsEnt):aux)

addListParMedHrNoPlan :: [(Medicamento, Horario)] -> PlanoMedicamento -> PlanoMedicamento
addListParMedHrNoPlan [] plan = plan
addListParMedHrNoPlan ((med,hr):xs) plan = addListParMedHrNoPlan xs (addParMedHrNoPlan (med,hr) plan [])
-- addListParMedHrNoPlan ((medEnt, hrEnt):ys) ((hr, med:meds):xs) = if hrEnt == hr
--                                                 then addListParMedHrNoPlan ys ((hrEnt, medEnt:(med:meds):xs))
--                                                         else addListParMedHrNoPlan ys ((hrEnt, medEnt:medsEnt):xs) ((hrEnt, medEnt:medsEnt):aux)

-- montaPlano :: Receituario -> PlanoMedicamento -> PlanoMedicamento
-- montaPlano [] planAux = planAux
-- montaPlano [(med, hr:hrs)] =  if (isElem hr myQuickSort (montaListaHrDeListaDePar (montaParMedHrRec (med, hr:hrs)) []))
-- then 


geraPlanoReceituario :: Receituario -> PlanoMedicamento
geraPlanoReceituario receit = addListParMedHrNoPlan (montaParMedHrRec receit) (montaPlanoVazio (myQuickSortAsc (montaListaHrDeListaDePar (montaParMedHrRec receit) [])) [])

myQuickSortDesc :: (Ord a) => [a] -> [a]
myQuickSortDesc [] = []
myQuickSortDesc (x:xs) = myQuickSortDesc [y | y <- xs, y >= x] ++ [x] ++ myQuickSortDesc [y|y <- xs , y < x]

myQuickSortAsc :: (Ord a) => [a] -> [a]
myQuickSortAsc [] = []
myQuickSortAsc (x:xs) = myQuickSortAsc [y | y <- xs, y < x] ++ [x] ++ myQuickSortAsc [y|y <- xs , y >= x]