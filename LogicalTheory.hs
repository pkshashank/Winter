{-# LANGUAGE GADTs #-}

module LogicalTheory where

data ExTypes where
  E :: ExTypes
  T :: ExTypes
  Arrow :: ExTypes -> ExTypes -> ExTypes
  Set :: ExTypes -> ExTypes
    deriving (Eq)

instance Show ExTypes where
    show E = "e"
    show T = "t"
    show (Arrow t1 t2) = "(" ++ show t1 ++ show t2 ++ ")"
    show (Set t) = "S" ++ show t


-- checks if a type is a boolean type
isBooleanType :: ExTypes -> Bool
isBooleanType t = case t of
    T -> True
    Arrow _ t1 -> isBooleanType t1
    _ -> False

isValueType :: ExTypes -> Bool
isValueType t = case t of
    T -> True
    Arrow _ T -> True
    _ -> False


data LambdaTerm where
    Var :: (Int, ExTypes) -> LambdaTerm
    App :: LambdaTerm -> LambdaTerm -> LambdaTerm
    Lam :: (Int, ExTypes) -> LambdaTerm -> LambdaTerm
    Const :: (String, ExTypes) -> LambdaTerm
    ForAll :: (Int, ExTypes) -> LambdaTerm -> LambdaTerm
    Exists :: (Int, ExTypes) -> LambdaTerm -> LambdaTerm
    deriving (Eq)

instance Show LambdaTerm where
    show (App (App op l1) l2) = if isInfix op then "(" ++ show l1 ++ " " ++ show op ++ " " ++ show l2 ++ ")" else "(" ++ show op ++ " " ++ show l1 ++ " " ++ show l2 ++ ")"
    show (Var (x, _)) = "x" ++ show x
    show (App t1 t2) = "(" ++ show t1 ++ " " ++ show t2 ++ ")"
    show (Lam (x, t) t1) = "(λ" ++ "x" ++ show x  ++ ":" ++ show t ++ "." ++ show t1 ++ ")"
    show (Const (x, t)) = x
    show (ForAll (x, t) t1) = "(∀" ++ "x" ++  show x ++ ":" ++ show t ++ "." ++ show t1 ++ ")"
    show (Exists (x, t) t1) = "(∃" ++ "x" ++ show x ++ ":" ++ show t ++ "." ++ show t1 ++ ")"


isInfix :: LambdaTerm -> Bool
isInfix (Const ("&", Arrow T (Arrow T T))) = True
isInfix (Const ("|", Arrow T (Arrow T T))) = True
isInfix (Const ("->", Arrow T (Arrow T T))) = True
isInfix (Const ("∈", Arrow _ (Arrow (Set _) T))) = True
isInfix (Const ("⊆", Arrow (Set _) (Arrow (Set _) T))) = True
isInfix (Const ("≠", Arrow _ (Arrow _ T))) = True
isInfix (Const ("=", Arrow _ (Arrow _ T))) = True
isInfix _ = False


-- Returning the type of a term
typeOf :: LambdaTerm -> Either String ExTypes
typeOf (Var (_, t))     = Right t
typeOf (Const (_, t))   = Right t
typeOf (Lam (_, t) t1)  = Arrow t <$> typeOf t1
typeOf (ForAll _ t1)    = typeOf t1 *> Right T
typeOf (Exists _ t1)    = typeOf t1 *> Right T
typeOf (App t1 t2)      = case typeOf t1 of
    Right (Arrow t1' t2') -> if typeOf t2 == Right t1'
                             then Right t2'
                             else Left "Function type mismatch"
    _ -> Left "Left argument not a function"



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
oneBeta :: LambdaTerm ->  LambdaTerm
oneBeta term@(App (Lam (x, tx) t1) t2)
    | typeOf t2 == Right tx = subst t1 x t2
    | otherwise             = term
oneBeta (App t1 t2) = App (oneBeta t1) (oneBeta t2)
oneBeta (Lam (x, tx) t1) = Lam (x, tx) (oneBeta t1)
oneBeta (ForAll (x, tx) t1) = ForAll (x, tx) (oneBeta t1)
oneBeta (Exists (x, tx) t1) = Exists (x, tx) (oneBeta t1)
oneBeta (Var (x, tx)) = Var (x, tx)
oneBeta (Const (x, tx)) = Const (x, tx)


-- Beta reduction
beta :: LambdaTerm -> LambdaTerm
beta term = let term' = oneBeta term in
                        if  term == term' then term else beta term'

betaError :: Either String LambdaTerm -> Either String LambdaTerm
betaError (Right t) = Right (beta t)
betaError (Left s) = Left s
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
