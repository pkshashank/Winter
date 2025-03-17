{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}


module Operators where
import LogicalLexicon
import LogicalTheory


-- Propositional Logic Operators
andT :: LambdaTerm -> LambdaTerm -> Either String LambdaTerm
andT x y
    | typeOf x == Right T, typeOf y == Right T = Right $ App (App (Const ("&", Arrow T (Arrow T T))) x) y
    | otherwise                                = Left "Types not T for and"


orT :: LambdaTerm -> LambdaTerm -> Either String LambdaTerm
orT x y
    | typeOf x == Right T, typeOf y == Right T = Right $ App (App (Const ("|", Arrow T (Arrow T T))) x) y
    | otherwise                                = Left "Types not T for or"

negT :: LambdaTerm -> Either String LambdaTerm
negT x
    | typeOf x == Right T = Right $ App (Const ("¬", Arrow T T)) x
    | otherwise           = Left "Type not T for not"


impT :: LambdaTerm -> LambdaTerm -> Either String LambdaTerm
impT x y
    | typeOf x == Right T, typeOf y == Right T = Right $ App (App (Const ("->", Arrow T (Arrow T T))) x) y
    | otherwise                                = Left "Types not T for imp"


-- Montague Raising Operator
m :: LambdaTerm -> Either String LambdaTerm
m x = case typeOf x of
    Right tx -> let p =  newVar x
                in let tp = (Arrow tx T)
                in Right $ Lam (p, tp) (App (Var (p, tp)) x)
    Left _ -> Left "Type not found"


-- Whether both types are boolean types and are same
booleanOperationPossible :: LambdaTerm -> LambdaTerm -> Bool
booleanOperationPossible l1 l2 = case (typeOf l1, typeOf l2) of
    (Right t1, Right t2) -> isBooleanType t1 && isBooleanType t2 && t1 == t2
    _ -> False


-- Polymorphic Boolean Operators
meet :: LambdaTerm -> LambdaTerm -> Either String LambdaTerm
meet l1 l2
    | typeOf l1 == Right T = andT l1 l2
    | Right (Arrow sig1 sig2) <- typeOf l1 = let z = newVar (App l1 l2)
        in Lam (z, sig1) <$> meet (bApp l1 (Var (z, sig1))) (bApp l2 (Var (z, sig1)))
    | otherwise = Left "Check dfn of meet, this should never happen"

join :: LambdaTerm -> LambdaTerm -> Either String LambdaTerm
join l1 l2
    | typeOf l1 == Right T = orT l1 l2
    | Right (Arrow sig1 sig2) <- typeOf l1 = let z = newVar (App l1 l2)
        in Lam (z, sig1) <$> join (bApp l1 (Var (z, sig1))) (bApp l2 (Var (z, sig1)))
    | otherwise = Left "Check dfn of join, this should never happen"

neg :: LambdaTerm -> Either String LambdaTerm
neg l
    |  Right t <- typeOf l, not (isBooleanType t) = Left "Not boolean type"
    | typeOf l == Right T = negT l
    | Right (Arrow sig1 sig2) <- typeOf l =
        let z = newVar l
        in Lam (z, sig1) <$> neg (bApp l (Var (z, sig1)))
    | otherwise = Left "Check dfn of not, this should never happen"

imp :: LambdaTerm -> LambdaTerm -> Either String LambdaTerm
imp l1 l2
    | typeOf l1 == Right T = impT l1 l2
    | Right (Arrow sig1 sig2) <- typeOf l1 = let z = newVar (App l1 l2)
        in ForAll (z, sig1) <$> imp (bApp l1 (Var (z, sig1))) (bApp l2 (Var (z, sig1)))
    | otherwise = Left "Check dfn of imp, this should never happen"



meetMontague :: LambdaTerm -> LambdaTerm -> Either String LambdaTerm
meetMontague l1 l2
    | booleanOperationPossible l1 l2 = meet l1 l2
    | Right ml1 <- m l1,  booleanOperationPossible (ml1) l2 = meet (ml1) l2
    | Right ml2 <- m l2, booleanOperationPossible l1 ml2 = meet l1 ml2
    | Right ml1 <- m l1, Right ml2 <- m l2 = meet ml1 ml2

joinMontague :: LambdaTerm -> LambdaTerm -> Either String LambdaTerm
joinMontague l1 l2
    | booleanOperationPossible l1 l2 = join l1 l2
    | Right ml1 <- m l1,  booleanOperationPossible (ml1) l2 = join (ml1) l2
    | Right ml2 <- m l2, booleanOperationPossible l1 ml2 = join l1 ml2
    | Right ml1 <- m l1, Right ml2 <- m l2 = join ml1 ml2

-- Set Theoretic Operators
belongs :: LambdaTerm -> LambdaTerm -> Either String LambdaTerm
belongs x y 
    | Right t <- typeOf x, Right (Set t') <- typeOf y, t == t' = Right $ bApp (bApp (Const ("∈", Arrow t (Arrow (Set t) T))) x) y
    | otherwise = Left "∈ not defined for these types"

subseteq :: LambdaTerm -> LambdaTerm -> Either String LambdaTerm
subseteq x y
    | Right (Set t) <- typeOf x, Right (Set t') <- typeOf y, t == t' = Right $ bApp (bApp (Const ("⊆", Arrow (Set t) (Arrow (Set t) T))) x) y
    | otherwise = Left "⊆ not defined for these types"


--upArrow which maps a set to the corresponding characteristic function
upArrow :: LambdaTerm -> Either String LambdaTerm
upArrow q = case typeOf q of
    Right (Set t) -> do
        let x = newVar q
        b' <- belongs (Var (x, t)) q
        Right $ Lam (x, t) b'
    _ -> Left "Type not Set"

min :: Either String LambdaTerm -> Either String LambdaTerm -> Either String LambdaTerm
min p a 
    | Right p' <- p , Right a' <- a = case (typeOf p', typeOf a') of
        (Right (Arrow (Arrow E T) T), Right (Set E)) -> do
            upA <- upArrow a'
            let b = newVar (App p' a')
            upB <- upArrow (Var (b, Set E))
            subBA <- subseteq a' (Var (b, Set E))
            bod <- impT (bApp p' upB) subBA
            andT (bApp p' upA) (ForAll (b, Set E) bod)
        _ -> Left "Can't apply min to these types"
    | otherwise = Left "Not Lambda Terms for min"

existentialRaise :: LambdaTerm -> LambdaTerm -> Either String LambdaTerm
existentialRaise q p 
    | Right (Arrow (Arrow E T) T) <- typeOf q, Right (Arrow (Set E) T) <- typeOf p = do
        let x = newVar (App p q)
        mnq <- Operators.min (Right q) (Right (Var (x, Set E)))
        bod <- andT (bApp p (Var (x, Set E))) mnq
        Right $ Exists (x, Set E) bod
    | otherwise = Left "Can't apply E to these types"


pl :: LambdaTerm -> Either String LambdaTerm
pl p 
    | Right (Arrow E T) <- typeOf p = do
        let q = newVar p
        let x = newVar (App p (Var (q, Set E)))
        neqstm <- neq (Var (q, Set E)) (emptySet (Set E))
        belStm <- belongs (Var (x, E)) (Var (q, Set E))
        impStm <- impT belStm (bApp p (Var (x, E)))
        andStm <- andT neqstm (ForAll (x, E) impStm)
        Right $ Lam (q, Set E) andStm
    | otherwise = Left "Can't apply plural operator to this type"