import PGF
import Math
import Data.Maybe (fromJust)
import Translation
import PrettyPrinter (prettyPrint)
import System.Process (callCommand)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO



main :: IO ()
main = do
  content <- readFile "test.txt"

  pgf <- readPGF "Math.pgf"
  let lang =  head $ languages pgf


  let pgfTree = head $ parse pgf lang (fromJust $ readType "Sentence") content
  producePNG pgf pgfTree
  
  putStr "1. PARSE TREE (ast.png): "
  putStrLn (showExpr [mkCId "Sentence"] pgfTree)
  let haskellTree = Math.fg pgfTree :: Math.GSentence
  putStrLn "2. Performing semantic translation..."
  
  formula <- translate haskellTree
  print $ prettyPrint formula
  return ()


producePNG :: PGF -> PGF.Tree -> IO ()
producePNG pgf pgfTree = do
  let graph = graphvizAbstractTree pgf (True, True) pgfTree
  TIO.writeFile "ast.dot" $ T.pack graph
  callCommand "dot -Tpng ast.dot -o ast.png"
  return ()