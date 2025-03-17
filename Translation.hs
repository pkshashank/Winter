{-# LANGUAGE GADTs #-}
module Translation where
import LogicalTheory
import LogicalLexicon
import Math
import Operators
import FlexibleApp



translateLexicon :: Math.Tree a -> IO LambdaTerm
translateLexicon expr =
    do
        putStr "Lexicon item: "
        case expr of
            GPrime -> print prime >> return prime
            GCoprime -> print coprime >> return coprime
            GInt i -> print (ltMkInt i) >> return (ltMkInt i)
            GInteger -> print integer >> return integer
            GReal_Number -> print realNumber >> return realNumber
            GCountable -> print countable >> return countable


translate :: Math.Tree a -> IO (Either String LambdaTerm)
translate expr = case expr of
    GSmkNPVP np vp -> do
                        Right rawNP <- translate np
                        Right rawVP <- translate vp
                        putStrLn ("S := Applying NP: " ++ show rawNP ++ " and VP: " ++ show rawVP ++ " getting " ++ show (flexApp (Right rawVP) (Right rawNP)))
                        return $ flexApp (Right rawVP) (Right rawNP)

    GNPconj conj np1 np2 -> do
                            Right rawNP1 <- translate np1
                            Right rawNP2 <- translate np2
                            let (op, label) = case conj of
                                    GAnd -> (meetMontague, "meet")
                                    GOr  -> (joinMontague, "join")
                            putStrLn $ "NP := " ++ label ++ " of NP1: " ++ show rawNP1 ++ " and NP2: " ++  show rawNP2 ++ " getting " ++ show (op rawNP1 rawNP2)
                            return (op rawNP1 rawNP2)
    GNPmkDetCN det cn -> do
                            Right rawCN <- translate cn
                            case det of
                                    GThe_Pl -> print  "plural The" 
                                    GThe_Sg -> print "singular The"
                            return (Left "not implemented")

    GNPmkInt i -> Right <$> translateLexicon i


    GVPmkAdj adj -> translate adj

    t -> Right <$> translateLexicon t -- This is a leaf node, so we just translate the lexicon
