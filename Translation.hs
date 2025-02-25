{-# LANGUAGE GADTs #-}
module Translation where
import LogicalTheory
import Math
import Operators
import FlexibleApp


translateLexicon :: Math.Tree a -> LambdaTerm
translateLexicon expr = case expr of
    GPrime -> prime
    GTwo -> two
    GThree -> three
    GCoprime -> coprime



translate :: Math.Tree a -> LambdaTerm
translate expr = case expr of
   GSmkNPVP np vp -> flexApp (translate vp) (translate np)
   GNPconj conj np1 np2 -> case conj of
                            GAnd -> meetMontague (translate np1) (translate np2)
                            GOr -> joinMontague (translate np1) (translate np2)
   GVPmkAdj adj -> translate adj 
   t -> translateLexicon t -- This is a leaf node, so we just translate the lexicon
    