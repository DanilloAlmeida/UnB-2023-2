str :: String
str = "Danillo"

firstDigit ::String -> Char
firstDigit str =    case (digits str) of
                    [] -> '\0'
                    (a:as) -> a