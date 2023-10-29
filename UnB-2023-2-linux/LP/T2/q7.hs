import ModeloDados
{-
   QUESTÃO 7  VALOR: 1,0 ponto

  Defina a função "geraPlanoReceituario", cujo tipo é dado abaixo e que, a partir de um receituario válido,
  retorne um plano de medicamento válido.

  Dica: enquanto o receituário lista os horários que cada remédio deve ser tomado, o plano de medicamentos  é uma
  disposição ordenada por horário de todos os remédios que devem ser tomados pelo paciente em um certo horário.

-}
extMed :: [(Medicamento, [Horario])] -> [Medicamento]
extMed [] = []
extMed ((med, hr):xs) = med:extMed xs

extHrs :: (Medicamento, [Horario]) -> [Horario]
extHrs (_ , []) = []
extHrs (med, hr:hrs) = hr : extHrs (med, hrs)

receituario4 :: Receituario
receituario4 = [(med4, [8, 9, 10, 17])]

isElem :: Horario -> [Horario] -> Bool
isElem _ [] = False
isElem hora (hr:hrs) = if hora == hr then True
                        else isElem hora hrs

montaParMedHr :: (Medicamento, [Horario]) -> [(Medicamento, Horario)]
montaParMedHr (_, []) = []
montaParMedHr (med, hr:hrs) = (med, hr):montaParMedHr (med, hrs)

montaParMedHrRec :: [(Medicamento, [Horario])] -> [(Medicamento, Horario)]
montaParMedHrRec [] = []
montaParMedHrRec ((med, hr:hrs): xs) = montaParMedHr (med, hr:hrs) ++ montaParMedHrRec xs

montaListaHrDeListaDePar :: [(Medicamento, Horario)] -> [Horario] -> [Horario]
montaListaHrDeListaDePar [] aux = aux
montaListaHrDeListaDePar ((med, hr):xs) aux =    if isElem hr aux
                                                            then montaListaHrDeListaDePar xs aux
                                                            else montaListaHrDeListaDePar xs (hr:aux)


myQuickSort :: (Ord a) => [a] -> [a]
myQuickSort [] = []
myQuickSort (x:xs) = myQuickSort [y | y <- xs, y < x] ++ [x] ++ myQuickSort [y|y <- xs , y >= x]

montaPlanoVazio :: Receituario -> PlanoMedicamento
montaPlanoVazio [(med, hrs)] = 

-- montaPlano :: Receituario -> PlanoMedicamento -> PlanoMedicamento
-- montaPlano [] planAux = planAux
-- -- montaPlano [(med, hr:hrs)] =  if (isElem hr myQuickSort (montaListaHrDeListaDePar (montaParMedHrRec (med, hr:hrs)) []))
-- --                               then 

addMedNoPlan :: Horario -> Medicamento -> PlanoMedicamento
addMedNoPlan hr med = [(hr, [med])]



-- geraPlanoReceituario :: Receituario -> PlanoMedicamento
-- geraPlanoReceituario [(_, [])] = []
-- geraPlanoReceituario [(med, (hr:hrs))] = (hr, [med]):geraPlanoReceituario [(med, hrs)]



