{-# LANGUAGE GADTs #-}
module Translation where
import LogicalTheory
import Math
import Operators
import FlexibleApp
import PrettyPrinter (Printable(prettyPrint))


translateLexicon :: Math.Tree a -> IO LambdaTerm
translateLexicon expr =
    do
        putStr "Lexicon item: "
        case expr of
            GPrime -> print prime >> return prime
            GCoprime -> print coprime >> return coprime
            GInt i -> print (ltMkInt i) >> return (ltMkInt i)


translate :: Math.Tree a -> IO LambdaTerm
translate expr = case expr of
   GSmkNPVP np vp ->    do
                        rawNP <- translate np
                        rawVP <- translate vp
                        print ("S := Applying " ++ prettyPrint rawNP ++ " and " ++ prettyPrint rawVP) 
                        return $ flexApp rawVP rawNP

   GNPconj conj np1 np2 ->  do
                            rawNP1 <- translate np1
                            rawNP2 <- translate np2
                            case conj of
                                GAnd -> print ("NP := Applying " ++ prettyPrint rawNP1 ++ " and " ++ prettyPrint rawNP2) >> return (meetMontague rawNP1 rawNP2)
                                GOr -> print ("NP := Applying " ++ prettyPrint rawNP1 ++ " or " ++ prettyPrint rawNP2) >> return (joinMontague rawNP1 rawNP2)
   GVPmkAdj adj -> translate adj
   GNPmkInt i -> translateLexicon i
   t -> translateLexicon t -- This is a leaf node, so we just translate the lexicon
