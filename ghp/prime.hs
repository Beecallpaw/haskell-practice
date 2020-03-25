factors :: Int -> [Int]
factors n = [x| x <- [1..n], n `mod` x == 0]

isPrime :: Int -> Bool
isPrime n = factors n == [1, n]

primesTill :: Int -> [Int]
primesTill n = [x| x <- [1..n], isPrime x]

smallerPrimeThan :: Int -> Int
smallerPrimeThan n = last $ primesTill n

greaterPrimeThan :: Int -> Int
greaterPrimeThan n = head [x| x <- [n+1..], isPrime x] 

