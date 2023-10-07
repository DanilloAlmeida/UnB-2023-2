type Person = String 
type Book = String
type Database = [(Person, Book)]

exampleBase = [("Alice", "Postman Pat"), 
               ("Anna", "All Alone"),
               ("Alice","Spot"), 
               ("Rory", "Postman Pat")]


books :: Database -> Person -> [Book]
books [] _ = []
books ((x,y):as) p  | x == p = y: books as p
                    |otherwise = books as p

borrowers :: Database -> Book -> [Person]
borrowers [] _ = []
borrowers ((x,y):as) b  | y == b = x: borrowers as b
                        | otherwise = borrowers as b

borrowed :: Database -> Book -> Bool
borrowed [] _ = False
borrowed ((x,y):as) b   | y == b = True
                        | otherwise = borrowed as b

numBorrowed :: Database -> Person -> Int
numBorrowed [] _ = 0
numBorrowed ((person, book):as) p       |  person == p = 1 + (numBorrowed as p)
                                        | otherwise = numBorrowed as p