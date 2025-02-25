{-# LANGUAGE GADTs #-}
module LogicalTheory where

data ExTypes where
  E :: ExTypes
  T :: ExTypes
  Arrow :: ExTypes -> ExTypes -> ExTypes
  Set :: ExTypes -> ExTypes
    deriving (Eq, Show)


-- checks if a type is a boolean type
isBooleanType :: ExTypes -> Bool
isBooleanType t = case t of
    T -> True
    Arrow _ t1 -> isBooleanType t1
    _ -> False


data LambdaTerm where
    Var :: (Int, ExTypes) -> LambdaTerm
    App :: LambdaTerm -> LambdaTerm -> LambdaTerm
    Lam :: (Int, ExTypes) -> LambdaTerm -> LambdaTerm
    Const :: (String, ExTypes) -> LambdaTerm
    ForAll :: (Int, ExTypes) -> LambdaTerm -> LambdaTerm
    Exists :: (Int, ExTypes) -> LambdaTerm -> LambdaTerm
    deriving (Eq, Show)

-- Returning the type of a term
typeOf :: LambdaTerm -> ExTypes
typeOf term = case term of
    Var (_, t) -> t
    App t1 t2 -> case typeOf t1 of
        (Arrow t1' t2') -> if t1' == typeOf t2 then t2' else error "Function type mismatch"
        _ -> error "Not a function type"
    Lam (_, t) t1 -> Arrow t (typeOf t1)
    Const (_, t) -> t
    ForAll (_, t) t1 -> T
    Exists (_, t) t1 -> T

-- Capture-avoiding substitution of a variable x with a term s in a term t
subst :: LambdaTerm -> Int -> LambdaTerm -> LambdaTerm
subst t x s = case t of
    Var (y, _) -> if x == y then s else t
    App t1 t2 -> App (subst t1 x s) (subst t2 x s)
    Lam (y, ty) tbod -> if x == y then t else Lam (y, ty) (subst tbod x s)
    Const _ -> t
    ForAll (y, ty) tbod -> if x == y then t else ForAll (y, ty) (subst tbod x s)
    Exists (y, ty) tbod -> if x == y then t else Exists (y, ty) (subst tbod x s)


-- One-step beta reduction
oneBeta :: LambdaTerm -> LambdaTerm
oneBeta term = case term of
    App (Lam (x, _) t1) t2 -> subst t1 x t2
    _ -> term

-- Beta reduction
beta :: LambdaTerm -> LambdaTerm
beta term = let term' = oneBeta term in
                        if term == term' then term else beta term'

-- Application with beta reduction
bApp :: LambdaTerm -> LambdaTerm -> LambdaTerm
bApp t1 t2 = beta (App t1 t2)

-- maxVar returns the maximum variable used in a term
maxVar :: LambdaTerm -> Int
maxVar term = case term of
    Var (x, _) -> x
    App t1 t2 -> max (maxVar t1) (maxVar t2)
    Lam (x, _) t1 -> max x (maxVar t1)
    Const _ -> 0
    ForAll (x, _) t1 -> max x (maxVar t1)
    Exists (x, _) t1 -> max x (maxVar t1)


-- Given a lambda term, find a new variable that is not used in the term
newVar :: LambdaTerm -> Int
newVar term = 1 + maxVar term


----------------------------------------
--- Logical Lexicon --------------------
----------------------------------------
two :: LambdaTerm
two = Const ("2", E)

three :: LambdaTerm
three = Const ("3", E)

prime :: LambdaTerm
prime = Const ("prime", Arrow E T)

coprime :: LambdaTerm
coprime = Const ("coprime", Arrow (Set E) T)

x = beta (App prime two)
