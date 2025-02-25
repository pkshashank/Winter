{-# LANGUAGE GADTs, FlexibleInstances, KindSignatures, RankNTypes, TypeSynonymInstances #-}
module Math where

import Control.Monad.Identity
import Data.Monoid
import PGF hiding (Tree)

----------------------------------------------------
-- automatic translation from GF to Haskell
----------------------------------------------------

class Gf a where
  gf :: a -> Expr
  fg :: Expr -> a

instance Gf GString where
  gf (GString x) = mkStr x
  fg t =
    case unStr t of
      Just x  ->  GString x
      Nothing -> error ("no GString " ++ show t)

instance Gf GInt where
  gf (GInt x) = mkInt x
  fg t =
    case unInt t of
      Just x  ->  GInt x
      Nothing -> error ("no GInt " ++ show t)

instance Gf GFloat where
  gf (GFloat x) = mkFloat x
  fg t =
    case unFloat t of
      Just x  ->  GFloat x
      Nothing -> error ("no GFloat " ++ show t)

----------------------------------------------------
-- below this line machine-generated
----------------------------------------------------

type GAdjective = Tree GAdjective_
data GAdjective_
type GConjunction = Tree GConjunction_
data GConjunction_
type GNounPhrase = Tree GNounPhrase_
data GNounPhrase_
type GSentence = Tree GSentence_
data GSentence_
type GVerbPhrase = Tree GVerbPhrase_
data GVerbPhrase_
type GString = Tree GString_
data GString_
type GInt = Tree GInt_
data GInt_
type GFloat = Tree GFloat_
data GFloat_

data Tree :: * -> * where
  GCoprime :: Tree GAdjective_
  GPrime :: Tree GAdjective_
  GAnd :: Tree GConjunction_
  GOr :: Tree GConjunction_
  GNPconj :: GConjunction -> GNounPhrase -> GNounPhrase -> Tree GNounPhrase_
  GThree :: Tree GNounPhrase_
  GTwo :: Tree GNounPhrase_
  GSmkNPVP :: GNounPhrase -> GVerbPhrase -> Tree GSentence_
  GVPmkAdj :: GAdjective -> Tree GVerbPhrase_
  GString :: String -> Tree GString_
  GInt :: Int -> Tree GInt_
  GFloat :: Double -> Tree GFloat_

instance Eq (Tree a) where
  i == j = case (i,j) of
    (GCoprime,GCoprime) -> and [ ]
    (GPrime,GPrime) -> and [ ]
    (GAnd,GAnd) -> and [ ]
    (GOr,GOr) -> and [ ]
    (GNPconj x1 x2 x3,GNPconj y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (GThree,GThree) -> and [ ]
    (GTwo,GTwo) -> and [ ]
    (GSmkNPVP x1 x2,GSmkNPVP y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GVPmkAdj x1,GVPmkAdj y1) -> and [ x1 == y1 ]
    (GString x, GString y) -> x == y
    (GInt x, GInt y) -> x == y
    (GFloat x, GFloat y) -> x == y
    _ -> False

instance Gf GAdjective where
  gf GCoprime = mkApp (mkCId "Coprime") []
  gf GPrime = mkApp (mkCId "Prime") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "Coprime" -> GCoprime 
      Just (i,[]) | i == mkCId "Prime" -> GPrime 


      _ -> error ("no Adjective " ++ show t)

instance Gf GConjunction where
  gf GAnd = mkApp (mkCId "And") []
  gf GOr = mkApp (mkCId "Or") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "And" -> GAnd 
      Just (i,[]) | i == mkCId "Or" -> GOr 


      _ -> error ("no Conjunction " ++ show t)

instance Gf GNounPhrase where
  gf (GNPconj x1 x2 x3) = mkApp (mkCId "NPconj") [gf x1, gf x2, gf x3]
  gf GThree = mkApp (mkCId "Three") []
  gf GTwo = mkApp (mkCId "Two") []

  fg t =
    case unApp t of
      Just (i,[x1,x2,x3]) | i == mkCId "NPconj" -> GNPconj (fg x1) (fg x2) (fg x3)
      Just (i,[]) | i == mkCId "Three" -> GThree 
      Just (i,[]) | i == mkCId "Two" -> GTwo 


      _ -> error ("no NounPhrase " ++ show t)

instance Gf GSentence where
  gf (GSmkNPVP x1 x2) = mkApp (mkCId "SmkNPVP") [gf x1, gf x2]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "SmkNPVP" -> GSmkNPVP (fg x1) (fg x2)


      _ -> error ("no Sentence " ++ show t)

instance Gf GVerbPhrase where
  gf (GVPmkAdj x1) = mkApp (mkCId "VPmkAdj") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "VPmkAdj" -> GVPmkAdj (fg x1)


      _ -> error ("no VerbPhrase " ++ show t)


instance Compos Tree where
  compos r a f t = case t of
    GNPconj x1 x2 x3 -> r GNPconj `a` f x1 `a` f x2 `a` f x3
    GSmkNPVP x1 x2 -> r GSmkNPVP `a` f x1 `a` f x2
    GVPmkAdj x1 -> r GVPmkAdj `a` f x1
    _ -> r t

class Compos t where
  compos :: (forall a. a -> m a) -> (forall a b. m (a -> b) -> m a -> m b)
         -> (forall a. t a -> m (t a)) -> t c -> m (t c)

composOp :: Compos t => (forall a. t a -> t a) -> t c -> t c
composOp f = runIdentity . composOpM (Identity . f)

composOpM :: (Compos t, Monad m) => (forall a. t a -> m (t a)) -> t c -> m (t c)
composOpM = compos return ap

composOpM_ :: (Compos t, Monad m) => (forall a. t a -> m ()) -> t c -> m ()
composOpM_ = composOpFold (return ()) (>>)

composOpMonoid :: (Compos t, Monoid m) => (forall a. t a -> m) -> t c -> m
composOpMonoid = composOpFold mempty mappend

composOpMPlus :: (Compos t, MonadPlus m) => (forall a. t a -> m b) -> t c -> m b
composOpMPlus = composOpFold mzero mplus

composOpFold :: Compos t => b -> (b -> b -> b) -> (forall a. t a -> b) -> t c -> b
composOpFold z c f = unC . compos (\_ -> C z) (\(C x) (C y) -> C (c x y)) (C . f)

newtype C b a = C { unC :: b }
