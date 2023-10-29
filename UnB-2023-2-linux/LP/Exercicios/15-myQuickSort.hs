lnum1:: [Int]
lnum1 =[1, 3, 5,2,8,2]

lstr1:: [String]
lstr1 = ["casa", "dado", "arvore", "dada"]

myQuickSort :: (Ord a) => [a] -> [a]
myQuickSort [] = []
myQuickSort (x:xs) = myQuickSort [y | y <- xs, y < x] ++ [x] ++ myQuickSort [y|y <- xs , y >= x]