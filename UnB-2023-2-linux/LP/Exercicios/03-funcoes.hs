square :: Int -> Int
square x = x*x

allEqual :: Int -> Int -> Int -> Bool
allEqual n m p = (n == m) && (m == p)

-- maxi :: Int -> Int
-- maxi 

addD :: Int -> Int -> Int
addD a b = 2 *(a + b)

fat :: Int -> Int
fat 0 = 1
fat a = fat (a-1) * a

fat2 n
    | n == 0 =1 
    | otherwise = n * fat2 (n-1)

-- all4Equal :: Int -> Int -> Int -> Int -> Bool
-- all4Equal n m  p q = allEqual

makeSpaces:: Int -> String
makeSpaces 0    = ""
makeSpaces n    
           | n >0 = " " ++ makeSpaces (n-1)