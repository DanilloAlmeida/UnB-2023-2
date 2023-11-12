tupla1:: (String, Int)
tupla1 = ("aaa", 1)

tupla2:: (String, Int)
tupla2 = ("bbb", 2)

l1 :: [Int]
l1 = [1,2,3,4,5,6]

l2 :: [Int]
l2 = [3,2,1,4,5,6]

l3 :: [(Int, [String])]
l3 = [(6,[""]),(5,[""]),(4,[""]),(1,["a1a1a1"]),(2,["bbb"]),(3,[""])]

listaParaListaDeTupla :: (String, Int) -> [Int] -> [(Int, String)] -> [(Int, String)]
listaParaListaDeTupla (_,_) [] aux = aux
listaParaListaDeTupla (str, num) (x:xs) aux = if (num == x)
                                            then listaParaListaDeTupla (str, num) xs ((num , str):aux)
                                            else listaParaListaDeTupla (str, num) xs ((x, ""):aux)

insereString :: (String, Int) -> [Int] -> [(Int, [String])] -> [(Int, [String])]
insereString (_,_) [] aux = aux
insereString (str, num) (x:xs) aux = if (num == x)
                                            then insereString (str, num) xs ((num , [str]):aux)
                                            else insereString (str, num) xs ((x, [""]):aux)

addString :: (String, Int) -> [(Int, [String])] -> [(Int, [String])] -> [(Int, [String])]
addString (_,_) [] aux = aux
addString (str, num) ((numEnt, strEnt:strsEnt):ents) aux = if (num == numEnt)
                                            then addString (str, num) ents ((num , str:(strEnt:strsEnt)):aux)
                                            else addString (str, num) ents ((numEnt, strEnt:strsEnt):aux)