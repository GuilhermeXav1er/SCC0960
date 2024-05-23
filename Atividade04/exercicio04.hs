import Data.List (intercalate)
import System.IO (isEOF)

-- Tipo Frame pode ter 2 ou 3 Inteiros
type Frame = [Int]

-- Função principal
main :: IO ()
main = do
    rolls <- fmap (map read . words) getLine
    let (frames, totalScore) = calculateBowlingScore rolls
    putStrLn $ formatFrames frames totalScore

-- Função para calcular a pontuação de boliche
calculateBowlingScore :: [Int] -> ([Frame], Int)
calculateBowlingScore rolls = processFrames rolls 0 [] 0
  where
    processFrames :: [Int] -> Int -> [Frame] -> Int -> ([Frame], Int)
    processFrames rolls rollIndex frames totalScore
        | length frames == 10 = (adjustFinalFrame frames rolls rollIndex, totalScore)
        | isStrike rolls rollIndex = 
            let score = 10 + rolls !! (rollIndex + 1) + rolls !! (rollIndex + 2)
                newFrame = [10, 0]  -- Representing X _ with [10, 0]
            in processFrames rolls (rollIndex + 1) (frames ++ [newFrame]) (totalScore + score)
        | isSpare rolls rollIndex = 
            let score = 10 + rolls !! (rollIndex + 2)
                newFrame = [rolls !! rollIndex, 10 - rolls !! rollIndex]  -- Representing spare
            in processFrames rolls (rollIndex + 2) (frames ++ [newFrame]) (totalScore + score)
        | otherwise = 
            let score = rolls !! rollIndex + rolls !! (rollIndex + 1)
                newFrame = [rolls !! rollIndex, rolls !! (rollIndex + 1)]
            in processFrames rolls (rollIndex + 2) (frames ++ [newFrame]) (totalScore + score)

    isStrike :: [Int] -> Int -> Bool
    isStrike rolls rollIndex = rolls !! rollIndex == 10

    isSpare :: [Int] -> Int -> Bool
    isSpare rolls rollIndex = rolls !! rollIndex + rolls !! (rollIndex + 1) == 10

    -- Ajusta o último frame para incluir jogadas extras, se houver
    adjustFinalFrame :: [Frame] -> [Int] -> Int -> [Frame]
    adjustFinalFrame frames rolls rollIndex = 
        if lastFrameIsStrikeOrSpare then init frames ++ [finalFrame] else frames
      where
        lastFrame = last frames
        lastFrameIsStrikeOrSpare = (head lastFrame == 10) || (sum lastFrame == 10)
        extraRolls = drop rollIndex rolls
        finalFrame
          | head lastFrame == 10 = [10] ++ take 2 extraRolls
          | sum lastFrame == 10  = lastFrame ++ take 1 extraRolls
          | otherwise            = lastFrame

-- Função para formatar os frames
formatFrames :: [Frame] -> Int -> String
formatFrames frames totalScore = intercalate " | " (map formatFrame (init frames) ++ [formatLastFrame (last frames)]) ++ " | " ++ show totalScore
  where
    formatFrame :: Frame -> String
    formatFrame [10, 0] = "X _"
    formatFrame [a, b] 
        | a + b == 10 = show a ++ " /"
        | otherwise = show a ++ " " ++ show b
    formatFrame [10, 0, c, d] = "X _ " ++ show c ++ " " ++ show d
    formatFrame [a, b, c] = show a ++ " / " ++ show c
    formatFrame _ = error "Invalid frame"

    formatLastFrame :: Frame -> String
    formatLastFrame [10, 10, 10] = "X X X"
    formatLastFrame [10, a, b] 
        | a + b == 10 = "X " ++ show a ++ " /"
        | otherwise = "X " ++ show a ++ " " ++ show b
    formatLastFrame [10, 10] = "X X"  -- Se ambos os lançamentos forem 10, mostramos "X X"
    formatLastFrame [a, b, 10] = show a ++ " / X"  -- Se o último frame for um spare seguido de um strike, mostramos "A / X"
    formatLastFrame frame = formatFrame frame  -- Reuse the regular format for other cases