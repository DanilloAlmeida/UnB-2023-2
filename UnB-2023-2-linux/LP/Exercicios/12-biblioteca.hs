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