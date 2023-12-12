chain::(Integral a) => a->[a]
chain 1 = [1]
chain x
    | even x = x: chain (x`div`2)
    | otherwise = x:chain (x*3 + 1)

numLongChains::Int
numLongChains = length (filter isLong (map chain[1..100]))
    where isLong xs = length xs > 20

lengthChain:: Int -> Int
lengthChain 1 = 1 
lengthChain x = length (chain x)

lengthChains :: [Int]
lengthChains = map length (map chain[1..100])

-- zip lengthChains [1..100]