checkSequence :: [Int] -> Int -> Int
checkSequence numbers index
  | index == length numbers - 1 = 1
  | numbers !! index < numbers !! (index + 1) = 1 + checkSequence numbers (index + 1)
  | otherwise = 1

getMaxLength :: [Int] -> Int -> Int -> Int
getMaxLength numbers maxLength currentIndex
  | currentIndex == length numbers = maxLength
  | otherwise = getMaxLength numbers newMaxLength newIndex
  where
    newCount = checkSequence numbers currentIndex
    newIndex = currentIndex + 1
    newMaxLength = max maxLength newCount

longestIncreasingSubsequence :: [Int] -> Int
longestIncreasingSubsequence numbers
  | null numbers = 0
  | length numbers == 1 = 1
  | otherwise = getMaxLength numbers 0 0

main :: IO ()
main = do
  inputStr <- getLine
  let numbers = map read (words inputStr) :: [Int]
  print $ longestIncreasingSubsequence numbers