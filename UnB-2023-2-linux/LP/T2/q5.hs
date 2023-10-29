import ModeloDados

{-
   QUESTÃO 5  VALOR: 1,0 ponto, sendo 0,5 para cada função.

 Um receituário é válido se, e somente se, todo os medicamentos são distintos e estão ordenados lexicograficamente e,
 para cada medicamento, seus horários também estão ordenados e são distintos.

 Inversamente, um plano de medicamentos é válido se, e somente se, todos seus horários também estão ordenados e são distintos,
 e para cada horário, os medicamentos são distintos e são ordenados lexicograficamente.

 Defina as funções "receituarioValido" e "planoValido" que verifiquem as propriedades acima e cujos tipos são dados abaixo:

 -}
-- myHead :: [Int] -> Int
-- myHead [x]   

-- validaHorarios :: [Int] -> Bool
-- validaHorarios [x] = True
-- validaHorarios (x:xs) | x >= head xs = False
--                       | otherwise = validaHorarios xs

extMed :: [(Medicamento, [Horario])] -> [Medicamento]
extMed [] = []
extMed ((med, hr):xs) = med:extMed xs

extHrs :: (Medicamento, [Horario]) -> [Horario]
extHrs (_ , []) = []
extHrs (med, hr:hrs) = hr : extHrs (med, hrs)

validaOrd :: (Ord a) => [a] -> Bool
validaOrd [x] = True
validaOrd (x:xs) | x >= head xs = False
                    | otherwise = validaOrd xs

receituarioValido :: Receituario -> Bool
receituarioValido [] = True
receituarioValido ((med, hr:hrs) : xs) =  ( validaOrd (extMed ((med, hr:hrs) : xs)) &&
                                                      validaOrd (extHrs (med, hr:hrs))
                                                    ) && receituarioValido xs

---------------------------------------------------------------------------------
-- planoValido :: PlanoMedicamento -> Bool
