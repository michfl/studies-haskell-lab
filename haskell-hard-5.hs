-- Dla danej liczby naturalnej n ≤ 999 podaj liczbę znaków w jej reprezentacji słownej. Dla n = 345 wynikiem jest 23, z tylu znaków składa się napis „trzysta czterdzieści pięć”.
import BasicFunctions


unityDigit = ["zero","jeden","dwa","trzy","cztery","piec","szesc","siedem","osiem","dziewiec"]
--              4       5      3     4       6       4       5       6        5        8
teenDigit = ["jedenascie","dwanascie","trzynascie","czternascie","pietnascie","szesnascie","siedemnascie","osiemnascie","dziewietnascie"]
--               10            9          10            11            10           10            12            11              14
tensDigit = ["dziesiec","dwadziescia","trzydziesci","czterdziesci","piecdziesiat","szescdziesiat","siedemdziesiat","osiemdziesiat","dziewiecdziesiat"]
--               8           11            11             12            12             13               14              13                16
hundredsDigit = ["sto","dwiescie","trzysta","czterysta","piecset","szescset","siedemset","osiemset","dziewiecset"]
--                 3       8          7          9          7         8           9           8           11


lengthOfElemInList :: (Num b) => [[a]] -> [b]
lengthOfElemInList x
     | length' x == 0 = []
lengthOfElemInList (x:xs) = length' x : lengthOfElemInList xs


unityDigitLength :: (Integral a) => Int -> a
unityDigitLength n
     | n < 0 = error "n must be >= 0"
     | n > 9 = error "n must be <= 9"
unityDigitLength n = lengthOfElemInList unityDigit `at` n


teenDigitLength :: (Integral a) => Int -> a
teenDigitLength n
     | n < 11 = error "n must be >= 11"
     | n > 19 = error "n must be <= 19"
teenDigitLength n = lengthOfElemInList teenDigit `at` (n-11)


tensDigitLength :: (Integral a) => Int -> a
tensDigitLength n
     | n < 10          = unityDigitLength n
     | n > 99          = error "n must be <= 99"
     | n `mod` 10 == 0 = lengthOfElemInList tensDigit `at` ((n `div` 10) - 1)
     | n <= 19         = teenDigitLength n
tensDigitLength n      = tensDigitLength ((n `div` 10) * 10) + unityDigitLength (n `mod` 10)


hundredsDigitLength :: (Integral a) => Int -> a
hundredsDigitLength n
     | n < 100          = tensDigitLength n
     | n > 999          = error "n must be <= 999"
     | n `mod` 100 == 0 = lengthOfElemInList hundredsDigit `at` ((n `div` 100) - 1)
hundredsDigitLength n   = hundredsDigitLength ((n `div` 100) * 100) + tensDigitLength (n `mod` 100)


main = do
     putStrLn "For a given natural n <= 999, find the number of characters used in its written representation"
     putStrLn "n value: "
     input <- getLine
     let n = (read input :: Int)
     print $ hundredsDigitLength n