reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

reverse'' :: [a] -> [a]
reverse'' [] = []
reverse'' (x:xs) = x: reverse'' xs 

repeat' :: a -> [a]
repeat' a = a:repeat' a