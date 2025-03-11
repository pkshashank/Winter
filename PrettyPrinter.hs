
{-# LANGUAGE FlexibleInstances #-}

module PrettyPrinter where
import LogicalLexicon
import LogicalTheory

class Printable a where
    prettyPrint :: a -> String

instance Printable ExTypes where
    prettyPrint = prettyPrintType

instance Printable LambdaTerm where
    prettyPrint = prettyPrintTerm . beta 

instance (Printable a) => Printable (Either String a) where
    prettyPrint (Right t) = prettyPrint t
    prettyPrint (Left s) = s  
-- Pretty printing of a Types
prettyPrintType :: ExTypes -> String
prettyPrintType t = case t of
    E -> "e"
    T -> "t"
    Arrow t1 t2 -> "(" ++ prettyPrintType t1 ++ " -> " ++ prettyPrintType t2 ++ ")"
    Set t1 -> "(S" ++ prettyPrintType t1 ++ ")"

-- Pretty printing of a LambdaTerm
prettyPrintTerm :: LambdaTerm -> String
prettyPrintTerm term = case term of
    Var (x, _) ->  "x" ++ show x
    App t1 t2 -> "(" ++ prettyPrint t1 ++ " " ++ prettyPrint t2 ++ ")"
    Lam (x, xt) t -> "(\\( x"++ show x ++ ":" ++ prettyPrintType xt ++ "), " ++ prettyPrint t ++ ")"
    Const (s, _) -> s
    ForAll (x, xt) t -> "(∀( x" ++ show x ++ ":" ++ prettyPrintType xt ++ "), " ++ prettyPrint t ++ ")"
    Exists (x, xt) t -> "(∃( x" ++ show x ++ ":" ++ prettyPrintType xt ++ "), " ++ prettyPrint t ++ ")"

