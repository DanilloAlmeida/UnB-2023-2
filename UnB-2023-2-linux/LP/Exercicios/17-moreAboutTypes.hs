removeNonUppercase::[Char] -> [Char]
removeNonUppercase l = [ c | c <- l, c `elem`  ['A'..'Z']]