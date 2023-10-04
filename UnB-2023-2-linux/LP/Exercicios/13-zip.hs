l1 = [1,1,1]
l2 = [2,2,2]
l3 = [3,3,3,3]

myZip:: [t] -> [u] -> [(t, u)]
myZip [] [] = []
myZip (a:as) (b:bs) = (a, b) : myZip as bs

myZipTuple :: t -> z -> (t,z)
myZipTuple a b = (a, b)