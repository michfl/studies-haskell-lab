module BasicFunctions
( sum'
, head'
, take'
, length'
, at
) where

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs


head' :: [a] -> a
head' [] = error "cannot apply to empty list"
head' (x:_) = x


take' :: (Integral a) => a -> [b] -> [b]
take' n _
    | n <= 0 = []
take' _ []   = []
take' n (x:xs) = x : take' (n-1) xs


length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs


at :: [a] -> Int -> a
at [] _ = error "access violation"
at (x:_) 0 = x
at (_:xs) n = at xs (n-1)