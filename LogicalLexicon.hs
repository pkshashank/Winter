module LogicalLexicon where
import LogicalTheory

ltMkInt :: Int -> LambdaTerm
ltMkInt n = Const (show n, E)

prime :: LambdaTerm
prime = Const ("prime", Arrow E T)

coprime :: LambdaTerm
coprime = Const ("coprime", Arrow (Set E) T)

countable :: LambdaTerm
countable = Const ("countable", Arrow (Set E) T)

emptySet :: ExTypes -> LambdaTerm
emptySet t = Const ("∅", t)

neq :: LambdaTerm -> LambdaTerm -> Either String LambdaTerm
neq l1 l2
    | Right t <- typeOf l1, Right t' <- typeOf l2, t == t' = Right $ bApp (bApp (Const ("≠", Arrow t (Arrow t T))) l1) l2
    | otherwise = Left "≠ not defined for these types"

integer :: LambdaTerm
integer = Const ("Z", Set E)

realNumber :: LambdaTerm
realNumber = Const ("R", Set E)

five :: LambdaTerm
five = ltMkInt 5

two :: LambdaTerm
two = ltMkInt 2

-- Example sets for testing
set1 :: LambdaTerm
set1 = Const ("{1,2,3}", Set E)

set2 :: LambdaTerm
set2 = Const ("{1,2,3,4}", Set E)

a :: LambdaTerm
a = Const ("a", Set E)

b = emptySet E