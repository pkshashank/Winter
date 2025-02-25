import PGF
import Math
import Data.Maybe (fromJust)
import Translation
import PrettyPrinter (prettyPrint)

main :: IO ()
main = do
  content <- readFile "test.txt"

  pgf <- readPGF "Math.pgf"
  let lang =  head $ languages pgf


  let pgfTree = head $ parse pgf lang (fromJust $ readType "Sentence") content
  let haskellTree = Math.fg pgfTree :: Math.GSentence
  putStr "Read and converted to haskell \n"
  
  let formula = translate haskellTree
  print $ prettyPrint formula

  return ()
