sum (takeWhile (< 5) (filter odd (map(^2)[1..100])))

(takeWhile(<900) [n^2 | n <- [1..], odd (n^2)])