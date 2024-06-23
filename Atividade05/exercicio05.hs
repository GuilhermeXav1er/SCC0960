-- Nome: Bruno Volpe        nUSP:14651980
-- Nome: Guilherme Xavier   nUSP:14575641

import Data.List
import Data.Ord (comparing)
import System.IO

data RegistroPais = RegistroPais
  { nomePais :: String,
    totalConfirmados :: Int,
    totalObitos :: Int,
    totalRecuperados :: Int,
    totalAtivos :: Int
  }
  deriving (Show)

parseLinhaCSV :: String -> RegistroPais
parseLinhaCSV linha =
  case split ',' linha of
    [c, con, d, r, a] -> RegistroPais c (read con) (read d) (read r) (read a)
    _ -> error $ "Formato de linha inválido: " ++ linha

split :: Char -> String -> [String]
split delim s = case dropWhile (== delim) s of
  "" -> []
  s' -> w : split delim s''
    where
      (w, s'') = break (== delim) s'

processaCSV :: String -> [RegistroPais]
processaCSV conteudo = map parseLinhaCSV $ filter (not . null) $ lines conteudo

carregaCSV :: FilePath -> IO [RegistroPais]
carregaCSV filePath = do
  conteudo <- readFile filePath
  return $ processaCSV conteudo

somaCasosAtivos :: Int -> [RegistroPais] -> Int
somaCasosAtivos limiar = sum . map totalAtivos . filter (\registro -> totalConfirmados registro >= limiar)

somaCasosObitos :: Int -> [RegistroPais] -> Int
somaCasosObitos quantidade = sum . map totalObitos . take quantidade . sortBy (comparing totalConfirmados)

paisesTopPorConfirmados :: Int -> [RegistroPais] -> [String]
paisesTopPorConfirmados quantidade = sort . map nomePais . take quantidade . sortBy (flip (comparing totalConfirmados))

main :: IO ()
main = do
  entrada <- getLine
  let [n1, n2, n3, n4] = map read $ words entrada :: [Int]

  dadosPaises <- carregaCSV "dados.csv"

  let somaCasosAtivosN1 = somaCasosAtivos n1 dadosPaises
  print somaCasosAtivosN1

  let registrosAtivosTopN2 = take n2 $ sortBy (flip (comparing totalAtivos)) dadosPaises
  let somaCasosObitosN3 = somaCasosObitos n3 registrosAtivosTopN2
  print somaCasosObitosN3

  let nomesPaisesOrdenados = paisesTopPorConfirmados n4 dadosPaises
  mapM_ putStrLn nomesPaisesOrdenados
