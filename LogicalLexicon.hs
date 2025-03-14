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

