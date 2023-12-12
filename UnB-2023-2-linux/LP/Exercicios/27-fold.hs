mySum :: (Num a)=> [a] -> a
mySum xs = foldl (\acc x -> acc + x) 0 xs