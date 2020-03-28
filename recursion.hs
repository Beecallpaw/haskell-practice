
length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + length xs

length'' :: [a] -> Int
length'' ls = lengthHelp ls 0

lengthHelp :: [a] -> Int -> Int
lengthHelp [] lenSoFar = lenSoFar
lengthHelp (_:xs) lenSoFar = lengthHelp xs (lenSoFar + 1)

length''' :: [a] -> Int
length''' = foldl (\x _-> x + 1) 0


sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

sum'' :: (Num a) => [a] -> a
sum'' ls = sumHelp ls 0

sumHelp :: (Num a) => [a] -> a -> a
sumHelp [] n = n
sumHelp (x:xs) n = sumHelp xs (x + n)

sum''' :: (Num a) => [a] -> a
sum''' = foldl (\x y -> x + y) 0


reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = (reverse' xs) ++ [x]

reverse'' :: [a] -> [a]
reverse'' ls = reverseHelp ls []

reverseHelp :: [a] -> [a] -> [a]
reverseHelp [] ls = ls
reverseHelp (x:xs) ls = reverseHelp xs (x : ls)

reverse''' :: [a] -> [a]
reverse''' = foldl (\x y -> y : x) []


elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' v (x:xs) | v == x = True
               | otherwise = elem' v xs


elem'' :: (Eq a) => a -> [a] -> Bool
elem'' v = foldl (\x y -> x || v == y) False  

take' :: Int -> [a] -> [a]
take' n _ | n <= 0 = []
take' v (x:xs) = x : take' (v-1) xs

drop' :: Int -> [a] -> [a]
drop' n xs | n <= 0 = xs
drop' n (x:xs) = drop (n-1) xs


