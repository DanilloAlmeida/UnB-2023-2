import Data.List
l1::[Int]
l1 = [3,4,2,5,9,2]

-- myFst :: [Int] -> Int
-- myFst [x] = x
-- myFst (x:xs) = x

getMenor :: [Int] -> Int
getMenor [x] = x
getMenor (x:xs) | x < getMenor xs = x
                | otherwise = getMenor xs

removeMenor :: [Int] -> [Int]
removeMenor [x] = []
removeMenor (x:xs)  | x == getMenor (x:xs) = xs
                    | otherwise = x: removeMenor xs

aux :: [Int] -> [Int] -> [Int]
aux listaOrdenada [] = listaOrdenada
aux listaOdenada (x:xs) = aux ( listaOdenada ++ [getMenor (x:xs)]) (removeMenor (x:xs))                   

mySort :: [Int] -> [Int]
mySort [] = []
mySort (x:xs) = aux [] (x:xs)