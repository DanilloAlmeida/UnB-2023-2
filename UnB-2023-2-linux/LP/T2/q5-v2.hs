import ModeloDados

extMedPlan :: (Horario, [Medicamento]) -> [Medicamento]
extMedPlan (_ , []) = []
extMedPlan (hr, med:meds) = med : extMedPlan (hr, meds)

extHrsPlan :: [(Horario, [Medicamento])] -> [Horario]
extHrsPlan [] = []
extHrsPlan ((hr, med):xs) = hr:extHrsPlan xs



validaOrd :: (Ord a) => [a] -> Bool
validaOrd [x] = True
validaOrd (x:xs) | x >= head xs = False
                    | otherwise = validaOrd xs
---------------------------------------------------------------------------------
planoValido :: PlanoMedicamento -> Bool
planoValido []=True
planoValido ((hr, med:meds) : xs) =  ( validaOrd (extHrsPlan ((hr, med:meds) : xs)) &&
                                                  validaOrd (extMedPlan (hr, med:meds))
                                                    ) && planoValido xs
