type Person = String 
type Book = String
type Database = [(Person, Book)]
-- type ExLoan01 = ()

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
<<<<<<< HEAD
                                        | otherwise = numBorrowed as p
=======
                                        | otherwise = numBorrowed as p

makeLoan :: Database -> Person -> Book -> Database
-- makeLoan [] = []
makeLoan db person book = (person, book):db

returnLoan :: Database -> Person -> Book -> Database
returnLoan [] p b = []
returnLoan ((person, book):as) p  b     | (p == person) && (b == book) = as
                                        | otherwise = (person, book) : returnLoan as p b
>>>>>>> 8e2f29ee34c659d2912d982916ac459dc671c7d5
