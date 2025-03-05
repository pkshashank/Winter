{-# LANGUAGE ImplicitParams #-}

module Operators where
import LogicalTheory
import PrettyPrinter

-- Propositional Logic Operators
andT :: LambdaTerm
andT = Const ("and", Arrow T (Arrow T T))

orT :: LambdaTerm
orT = Const ("or", Arrow T (Arrow T T))

negT :: LambdaTerm
negT = Const ("neg", Arrow T T)

impT :: LambdaTerm
impT = Const ("imp", Arrow T (Arrow T T))



-- Montague Raising Operator
m :: LambdaTerm -> LambdaTerm
m x =  let tp = Arrow (typeOf x) T in
                    let p = newVar x
                  in Lam (p, tp) (App (Var (p, tp)) x)

booleanOperationPossible :: LambdaTerm -> LambdaTerm -> Bool
booleanOperationPossible l1 l2 = case (typeOf l1, typeOf l2) of
    (t1, t2) -> t1 == t2 && isBooleanType t1 && isBooleanType t2


-- Polymorphic Boolean Operators
meet :: LambdaTerm -> LambdaTerm -> LambdaTerm
meet l1 l2 =  if booleanOperationPossible l1 l2
                    then case (typeOf l1, typeOf l2) of
                        (T, T) -> bApp (bApp andT l1) l2
                        (t1, t2) -> let z = newVar (App l1 l2)
                                    in case t1 of
                                        Arrow sig1 sig2 -> Lam (z, sig1) (meet (bApp l1 (Var (z, sig1))) (bApp l2 (Var (z, sig1))))
                                        _ -> error "Types boolean and same type, but not arrow" -- This should never happen as we have already checked for boolean types

                            else
                                error "Boolean Operation not possible"


join :: LambdaTerm -> LambdaTerm -> LambdaTerm
join l1 l2 = if booleanOperationPossible l1 l2
                    then case (typeOf l1, typeOf l2) of
                        (T, T) -> bApp (bApp orT l1) l2
                        (t1, t2) -> let z = newVar (App l1 l2)
                                    in case t1 of
                                        Arrow sig1 sig2 -> Lam (z, sig1) (join (bApp l1 (Var (z, sig1))) (bApp l2 (Var (z, sig1))))
                                        _ -> error "Types boolean and same type, but not arrow" -- This should never happen as we have already checked for boolean types

                            else
                                error "Boolean Operation not possible"


neg :: LambdaTerm -> LambdaTerm
neg l = if isBooleanType (typeOf l) then
            case typeOf l of
                T -> bApp negT l
                t -> let z = newVar l
                    in case t of
                        Arrow sig1 sig2 -> Lam (z, sig1) (neg (bApp l (Var (z, sig1))))
                        _ -> error "Types boolean and same type, but not arrow" -- This should never happen as we have already checked for boolean types
        else
            error "Negation not possible"

imp :: LambdaTerm -> LambdaTerm -> LambdaTerm
imp l1 l2 = if booleanOperationPossible l1 l2 then
                case (typeOf l1, typeOf l2) of
                    (T, T) -> bApp (bApp impT l1) l2
                    (t1, t2) -> let z = newVar (App l1 l2)
                                in case t1 of
                                    Arrow sig1 sig2 -> ForAll (z, sig1) (imp (bApp l1 (Var (z, sig1))) (bApp l2 (Var (z, sig1))))
                                    _ -> error "Types boolean and same type, but not arrow" -- This should never happen as we have already checked for boolean types
            else
                error "Boolean Operation not possible"

zero :: ExTypes -> LambdaTerm
zero t = meet (neg (Var (0, t))) (Var (0, t))

one :: ExTypes -> LambdaTerm
one t = join (neg (Var (0, t))) (Var (0, t))


meetMontague :: LambdaTerm -> LambdaTerm -> LambdaTerm
meetMontague l1 l2
  | booleanOperationPossible l1 l2 = meet l1 l2
  | booleanOperationPossible (m l1) l2 = meet (m l1) l2
  | booleanOperationPossible l1 (m l2) = meet l1 (m l2)
  | otherwise = meet (m l1) (m l2)

joinMontague :: LambdaTerm -> LambdaTerm -> LambdaTerm
joinMontague l1 l2
  | booleanOperationPossible l1 l2 = join l1 l2
  | booleanOperationPossible (m l1) l2 = join (m l1) l2
  | booleanOperationPossible l1 (m l2) = join l1 (m l2)
  | otherwise = join (m l1) (m l2)




--min :: LambdaTerm -> LambdaTerm -> LambdaTerm -- Type: ((S^n(e) t) t) (S^(n+1)(e) t)
--min l1 l2 = case (typeOf l1, typeOf l2) of
--    Arrow (snType n) t, Arrow (snType (n+1)) t -> meetMontague l1 l2
--   _ -> error "Types do not match"