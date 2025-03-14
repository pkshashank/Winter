module FlexibleApp where
import LogicalTheory
import LogicalLexicon
import Operators
import Utilities


-- This is where we define applications.
-- The theory being, two lambda terms are applied to each other, unless there is a type mismatch.
-- Otherwise, we use operators to resolve the type mismatch

-- Checks if the types of two terms clash when doing application 
noTypeClash :: LambdaTerm -> LambdaTerm -> Bool
noTypeClash t1 t2 = case (typeOf t1, typeOf t2) of
    (Right (Arrow t1' t2'), Right t3') -> t1' == t3'
    _ -> False

appWithNoTypeClash ::  LambdaTerm ->  LambdaTerm -> Either String LambdaTerm
appWithNoTypeClash t1 t2
  | noTypeClash t1 t2 = Right (bApp t1 t2)
  | otherwise = Left "Type Mismatch"


-- Applies term t1 to t2 each other.
-- First tries t1 t2, then t2 t1.
-- Then prints an error if none of them is possible
flexApp :: Either String LambdaTerm -> Either String LambdaTerm -> Either String LambdaTerm
flexApp t1 t2
  | Right t1' <- t1, Right t2' <- t2 = appWithNoTypeClash t1' t2' <||> 
    appWithNoTypeClash t2' t1' <||> 
    existentialRaise t2' t1' <||> -- this is used in the coprime example
    existentialRaise t1' t2' <||>
    Left "Application Failed, tried E"
  | otherwise = Left "One of the lambda terms is wrong in flexApp"
    

