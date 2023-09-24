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