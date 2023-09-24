type Person = String
type Book = String
type Database = [(Person, Book)]

exampleBase = [("Alice", "Postman Pat"), ("Anna", "All Alone"), ("Alice","Spot"), ("Rory", "Postman Pat")]

books :: Database -> Person -> [Book]
books [] p = []
books (a:as) p  | fst a == p = (snd a): books as p
                | otherwise = books as p

borrowers :: Database -> Book -> [Person]
borrowers [] b = []
borrowers (a:as) b  | snd a == b = (fst a): borrowers as b
                    | otherwise = borrowers as b 

borrowed :: Database -> Book -> Bool
borrowed db b = if (borrowers db b) /= []
                then True
                else False
-- numBorrowed :: Database -> Person -> Int