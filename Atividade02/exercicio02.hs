isPrime :: Integer -> Integer -> Bool
isPrime n divisor
    | n <= 1 = False
    | divisor * divisor > n = True
    | n `mod` divisor == 0 = False
    | otherwise = isPrime n (divisor + 1)

primeNumbers :: Integer -> Integer -> [Integer]
primeNumbers x y
    | x > y = []
    | isPrime x 2 = x : primeNumbers (x + 1) y
    | otherwise = primeNumbers (x + 1) y

primeIntervals :: [Integer] -> [Integer]
primeIntervals primeList
    | length primeList < 2 = []
    | otherwise = (head (tail primeList) - head primeList) : primeIntervals (tail primeList)

longestInterval :: [Integer] -> Integer
longestInterval intervals
    | length intervals == 0 = 0
    | length intervals == 1 = head intervals
    | otherwise = max (head intervals) (longestInterval (tail intervals))

main :: IO ()
main = do
    xStr <- getLine
    yStr <- getLine
    let x = read xStr :: Integer
        y = read yStr :: Integer
        primeList = primeNumbers x y
        intervals = primeIntervals primeList
        maxInterval = longestInterval intervals
    print maxInterval
