-- Dla danej liczby naturalnej n wyświetl pierwszą liczbę trójkątną (suma kolejnych liczb naturalnych) która posiada więcej niż n dzielników.
-- Np. siódma liczba trójkątna 28 = 1 + 2 + 3 + 4 + 5 + 6 + 7 posiada 6 dzielników: 1, 2, 4, 7, 14, 28

import BasicFunctions


triangularNumber :: (Integral a) => Int -> a
triangularNumber n
   | n > 0    = sum' (take' n [1,2..])
   | otherwise = error "n must be > 0"


listOfDivisors :: (Integral a) => a -> [a]
listOfDivisors n 
   | n > 0     = [ x | x <- [1..n], n `mod` x == 0 ]
   | otherwise = error "n must be > 0"


findDivTriangularNumber :: (Integral a) => Int -> a
findDivTriangularNumber n
   | n > 0    = head' [ triangularNumber x | x <- [1..], len x > n ]
   | otherwise = error "n must be natural"
   where len = length' . listOfDivisors . triangularNumber


main = do
   putStrLn "For a given natural n, find the first triangular number that has more than n divasors."
   putStrLn "n value: "
   input <- getLine
   let n = (read input :: Int)
   print $ findDivTriangularNumber n