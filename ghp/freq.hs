import Data.Char

lowers :: [Char] -> Int
lowers xs = length [x | x<-xs, isLower x ]

count :: Char -> [Char] -> Int
count x xs = length [x' | x'<-xs, x==x']

percent :: Int -> Int -> Int 
percent x y = round $ (fromIntegral x / fromIntegral y) * 100

freqs :: String -> [(Char, Int)]
freqs xs = [(x, percent (count x xs) n )| x <- ['a'..'z']]
            where n = lowers xs
