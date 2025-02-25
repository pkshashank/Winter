module FlexibleApp where
import LogicalTheory
import Operators
import GF.Support (err)

-- This is where we define applications.
-- The theory being, two lambda terms are applied to each other, unless there is a type mismatch.
-- Otherwise, we use operators to resolve the type mismatch

-- Checks if the types of two terms clash when doing application 
noTypeClash :: LambdaTerm -> LambdaTerm -> Bool
noTypeClash t1 t2 = case (typeOf t1, typeOf t2) of
    (Arrow t1' t2', t3') -> t1' == t3'
    _ -> False

-- Applies term t1 to t2 each other.
-- First tries t1 t2, then t2 t1.
-- Then prints an error if none of them is possible
unorderedApp :: LambdaTerm -> LambdaTerm -> Maybe LambdaTerm
unorderedApp t1 t2
  | noTypeClash t1 t2 = Just (bApp t1 t2)
  | noTypeClash t2 t1 = Just (bApp t2 t1)
  | otherwise = Nothing


flexApp :: LambdaTerm -> LambdaTerm -> LambdaTerm
flexApp t1 t2 = case unorderedApp t1 t2 of
    Just t -> t
    Nothing -> error "Unordered Application failed"