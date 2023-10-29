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
montaListaHrDeListaDePar ((med, hr):xs) (hrAux:hrsAux) =    if (isElem hr (hrAux:hrsAux))
                                                            then montaListaHrDeListaDePar xs (hrAux:hrsAux)
                                                            else hr : montaListaHrDeListaDePar xs (hrAux:hrsAux)

-- montaPlanoDePar ::  [(Medicamento, Horario)] -> [(Horario, [Medicamento])] -> [(Horario, [Medicamento])]
-- montaPlanoDePar [] _ = []
-- montaPlanoDePar ((med, hr):xs) (hrPlan, medsPlan) = if hr == hrPlan 
--                                                     then montaPlanoDePar xs [(hrPlan, med:medsPlan)]
--                                                     else 

-- geraPlanoReceituario :: Receituario -> PlanoMedicamento
-- geraPlanoReceituario [(_, [])] = []
-- geraPlanoReceituario [(med, (hr:hrs))] = (hr, [med]):geraPlanoReceituario [(med, hrs)]



-- geraPlanoReceituario [(_, []):xs] = geraPlanoReceituario [(med, hrs):xs]
-- geraPlanoReceituario [(med, hr:hrs): xs] = (hr, [med]) : geraPlanoReceituario [(med, hrs)]