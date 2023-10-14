import ModeloDados
{-   QUESTÃO 1, VALOR: 1,0 ponto

Defina a função "comprarMedicamento", cujo tipo é dado abaixo e que, a partir de um medicamento, uma quantidade e um estoque inicial de medicamentos, retorne um novo estoque de medicamentos contendo o medicamento adicionado da referida quantidade. Se o medicamento já existir na lista de medicamentos, então a sua quantidade deve ser atualizada no novo estoque.
Caso o remédio ainda não exista no estoque, o novo estoque a ser retornado deve ter o remédio e sua quantidade como cabeça.

-}
inverteCabeca :: EstoqueMedicamentos -> EstoqueMedicamentos
inverteCabeca (e:es) = es ++ [e]

adicionaMedicamento :: Medicamento -> Quantidade -> EstoqueMedicamentos -> EstoqueMedicamentos
adicionaMedicamento m q [] = [(m, q)]
adicionaMedicamento med qtd ((m,q):es)  |  m == med = (m, (q+qtd)):es
                                        | otherwise = (m,q) : adicionaMedicamento med qtd es

contaEstoque :: EstoqueMedicamentos -> Int
contaEstoque [x] = 1
contaEstoque (x:xs) = contaEstoque xs + 1

-- comprarMedicamento :: Medicamento -> Quantidade -> EstoqueMedicamentos -> EstoqueMedicamentos
-- comprarMedicamento = undefined