myIsEven :: Int -> Bool
myIsEven x  | (mod x 2 == 0) = True
            | otherwise = False

myIsEven2 :: Int -> Bool
myIsEven2 x =   if (mod x 2 == 0) 
                then True
                else False

doubleListIfEven :: [Int] -> [Int]
doubleListIfEven xs = [2*a|a <- xs, myIsEven a]