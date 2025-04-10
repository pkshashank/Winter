import PGF
import Math
import Data.Maybe (fromJust)
import Translation
import System.Process (callCommand)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO
import LogicalTheory
import System.Posix.Internals (puts)



main :: IO ()
main = do
  content <- readFile "test.txt"

  pgf <- readPGF "Math.pgf"
  let lang =  head $ languages pgf -- The language is called MathEng


  let pgfTrees = parse pgf lang (fromJust $ readType "Sentence") content
  let pgfTree = head pgfTrees
  let noOfTrees = length pgfTrees
  producePNG pgf pgfTree
  putStrLn "\nThese are all the parse trees. We work with the first one."
  showAllTrees pgfTrees

  putStr "\n1. PARSE TREE (ast.png): \n"
  putStrLn (showExpr [mkCId "Sentence"] pgfTree)
  let haskellTree = Math.fg pgfTree :: Math.GSentence
  putStrLn "\n2. PERFORMING SEMANTIC ANALYSIS..."

  formula <- translate haskellTree
  putStr "\n3. TRANSLATED FORMULA:\n"
  case formula of
    Left err -> print err
    Right f -> print f
  return ()


producePNG :: PGF -> PGF.Tree -> IO ()
producePNG pgf pgfTree = do
  let graph = graphvizAbstractTree pgf (True, True) pgfTree
  TIO.writeFile "ast.dot" $ T.pack graph
  callCommand "dot -Tpng ast.dot -o ast.png"
  return ()
  
showAllTrees :: [PGF.Tree] -> IO ()
showAllTrees pgfTrees = do
  case pgfTrees of
    [] -> return ()
    (pgfTree:pgfTrees) -> putStrLn (showExpr [mkCId "Sentence"] pgfTree) >> showAllTrees pgfTrees