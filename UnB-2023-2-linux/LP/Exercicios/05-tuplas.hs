-- p :: (Int, Int)
p = (33,22)

-- myFst:: (Int, Int) -> Int
myFst (a, b) = a

-- mySnd:: (Int, Int) -> Int
mySnd (_, b)= b

-- addPair:: (Int, Int) -> Int
-- addPair (x,y)=x+y
addPair x = myFst x + mySnd x