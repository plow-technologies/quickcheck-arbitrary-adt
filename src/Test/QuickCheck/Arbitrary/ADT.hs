{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TypeOperators     #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE ScopedTypeVariables #-}

module Test.QuickCheck.Arbitrary.ADT where

import           GHC.Generics

import           Test.QuickCheck.Gen
import           Test.QuickCheck.Arbitrary

-- | GArbitrary is a typeclass for generalizing the creation of single arbitrary
-- product and sum types. Assume a simple sum type
-- data SumType = SumTypeFirst  String
--              | SumTypeSecond String
--

class GArbitrary rep where
  garbitrary :: Gen (rep a)

instance GArbitrary U1 where
  garbitrary = pure U1

instance (GArbitrary l, GArbitrary r) => GArbitrary (l :+: r) where
  garbitrary = do
    b <- arbitrary
    if b then L1 <$> garbitrary
         else R1 <$> garbitrary

instance (GArbitrary l, GArbitrary r) => GArbitrary (l :*: r) where
  garbitrary = (:*:) <$> garbitrary <*> garbitrary

instance Arbitrary a => GArbitrary (K1 i a) where
  garbitrary = K1 <$> arbitrary

instance GArbitrary rep => GArbitrary (M1 i t rep) where
  garbitrary = M1 <$> garbitrary

genericArbitrary :: (Generic a, GArbitrary (Rep a)) => Gen a
genericArbitrary = to <$> garbitrary


-- | GArbitraryList is a typeclass for generalizing the creation of a list of
-- arbitrary instances of each constructor of a sum type. In the case of a
-- product type it will return a list of a single arbitrary. `garbitraryList` is
-- recurse through child sum types.


-- garbitraryList = (:[]) . (:*:) <$> garbitrary <*> garbitrary`

class GArbitraryWithCon rep where
  garbitraryWithCon :: Gen (String, rep a)

instance GArbitraryWithCon U1 where
  garbitraryWithCon = pure ("",U1)

instance (GArbitraryWithCon l, GArbitraryWithCon r) => GArbitraryWithCon (l :+: r) where
  garbitraryWithCon = do
    b <- arbitrary
    if b then fmap L1 <$> garbitraryWithCon
         else fmap R1 <$> garbitraryWithCon

instance (GArbitraryWithCon l, GArbitraryWithCon r) => GArbitraryWithCon (l :*: r) where
  garbitraryWithCon = do
    (a,x) <- garbitraryWithCon
    (b,y) <- garbitraryWithCon
    return ("", x :*: y)

instance Arbitrary a => GArbitraryWithCon (K1 i a) where
  garbitraryWithCon = (,) "" . K1 <$> arbitrary

instance (Constructor c, GArbitraryWithCon rep) => GArbitraryWithCon (M1 C c rep) where
  garbitraryWithCon = fmap M1 <$> ((,) <$> pure con <*> (snd <$> garbitraryWithCon))
    where
      con = conName (undefined :: M1 C c rep ())

instance GArbitraryWithCon rep => GArbitraryWithCon (M1 D t rep) where
  garbitraryWithCon = fmap M1 <$> garbitraryWithCon

instance GArbitraryWithCon rep => GArbitraryWithCon (M1 S t rep) where
  garbitraryWithCon = fmap M1 <$> garbitraryWithCon


genericArbitraryWithCon :: (Generic a, GArbitraryWithCon (Rep a)) => Gen (String, a)
genericArbitraryWithCon = fmap to <$> garbitraryWithCon






class GArbitraryList rep where
  garbitraryList :: Gen [(String, rep a)]

instance GArbitraryList U1 where
  garbitraryList = pure [("",U1)]

instance (GArbitraryList l, GArbitraryList r) => GArbitraryList (l :+: r) where
  garbitraryList = (++) <$> ((fmap . fmap) L1 <$> garbitraryList) <*> ((fmap . fmap) R1 <$> garbitraryList)

instance (GArbitraryList l, GArbitraryList r) => GArbitraryList (l :*: r) where
  garbitraryList = do
    xs <- garbitraryList
    ys <- garbitraryList
    return [("", (snd . head $ xs) :*: (snd . head $ ys))]
  --garbitraryList = (++) <$> ((fmap . fmap) L1 <$> garbitraryList) <*> ((fmap . fmap) R1 <$> garbitraryList)
  --garbitraryList = (fmap . fmap . fmap) (:*:) <$> garbitraryList <*> garbitraryList
  --garbitraryList = (fmap . fmap) (:*:) <$> (head <$> garbitraryList) <*> (head <$> garbitraryList)

  -- garbitraryList = (:[]) . (:*:) <$> garbitrary <*> garbitrary`
  --garbitraryList = fmap pure <$> garbitrary
--  garbitraryList = (:*:) <$> pure [("",)] <*> pure []

--instance Arbitrary a => GArbitraryList (K1 i a) where
--  garbitraryList = K1 <$> arbitrary --  fmap (:[]) . K1 <$> arbitrary

{-
instance (Constructor c) => GArbitraryList (M1 C c U1) where
  garbitraryList = (fmap . fmap) M1 <$> pure [(con , U1)]
    where
      con = conName (undefined :: M1 C c U1 ())
-}
-- (selName (undefined :: M1 C x U1 ())
--instance (Selector x , GArbitraryList rep) => GArbitraryList (M1 C x rep) where

instance (Constructor c, GArbitraryList rep) => GArbitraryList (M1 C c rep) where
  garbitraryList = (fmap . fmap) M1 . (:[]) <$> ((,) <$> pure con <*> (snd . head <$> garbitraryList))
    where
      con = conName (undefined :: M1 C c rep ())

{-
instance (Constructor c, Arbitrary a) => GArbitraryList (M1 C c (K1 z a)) where
  garbitraryList = (fmap . fmap) M1 <$> (:[]) <$> ((,) <$> pure con <*> (K1 <$> arbitrary))
    where
      con = conName (undefined :: M1 C c (K1 z a) ())
-}
--instance GArbitraryList f => GArbitraryList (M1 D x f) where
--  garbitraryList = garbitraryList

{-
instance (Constructor c, Selector s, Arbitrary a) =>  GArbitraryList (M1 C c (M1 S s (K1 z a))) where
  garbitraryList = (fmap . fmap) M1 . (:[]) <$> ((,) <$> pure con <*> (M1 . K1 <$> arbitrary))
      where
        --con = conName (undefined :: M1 C c () ())
        con = conName (undefined :: M1 C c (M1 S s (K1 z a)) ())
-}
instance Arbitrary a => GArbitraryList (K1 i a) where
  garbitraryList = (:[]) . (,) "" . K1 <$> arbitrary


instance GArbitraryList rep => GArbitraryList (M1 D t rep) where
  garbitraryList = (fmap . fmap) M1 <$> garbitraryList

instance GArbitraryList rep => GArbitraryList (M1 S t rep) where
  garbitraryList = (fmap . fmap) M1 <$> garbitraryList

--instance (Selector s, Arbitrary a) =>  GArbitraryList (M1 S s (K1 z a)) where
--  garbitraryList = (fmap . fmap) M1 <$> (:[]) . (,) "" . K1 <$> arbitrary
    --(fmap . fmap) M1 . (:[]) <$> ((,) <$> pure "" <*> (M1 . K1 <$> arbitrary))

{-
(GArbitraryList (M1 S NoSelector (Rec0 Int)))

instance Selectors f => Selectors (M1 D x f) where
  selectors _ = selectors (Proxy :: Proxy f)

instance Selectors f => Selectors (M1 C x f) where
  selectors _ = selectors (Proxy :: Proxy f)

instance (Selector s, Typeable t) => Selectors (M1 S s (K1 R t)) where
  selectors _ =
    [ ( selName (undefined :: M1 S s (K1 R t) ()) , typeOf (undefined :: t) ) ]
-}

genericArbitraryList :: (Generic a, GArbitrary (Rep a), GArbitraryList (Rep a)) => Gen [(String,a)]
genericArbitraryList = (fmap . fmap) to <$> garbitraryList

-- let y = (:[]) <$> ((,) <$> pure "asdf" <*> x)

{-
instance (Selector s, Typeable t) => Selectors (M1 S s (K1 R t)) where
  selectors _ =
    [ ( selName (undefined :: M1 S s (K1 R t) ()) , typeOf (undefined :: t) ) ]

-}

--instance GArbitraryList rep => GArbitraryList (M1 i t rep) where
--  garbitraryList = fmap M1 <$> garbitraryList

--genericArbitraryList :: (Generic a, GArbitrary (Rep a), GArbitraryList (Rep a)) => Gen [a]
--genericArbitraryList = fmap to <$> garbitraryList

{-
class GArbitraryList rep where
  garbitraryList :: Gen [rep a]

instance GArbitraryList U1 where
  garbitraryList = pure [U1]

instance (GArbitraryList l, GArbitraryList r) => GArbitraryList (l :+: r) where
  garbitraryList = (++) <$> (fmap L1 <$> garbitraryList) <*> (fmap R1 <$> garbitraryList)

instance (GArbitraryList l, GArbitrary l, GArbitraryList r, GArbitrary r) => GArbitraryList (l :*: r) where
  garbitraryList = pure <$> garbitrary

instance Arbitrary a => GArbitraryList (K1 i a) where
  garbitraryList = (:[]) . K1 <$> arbitrary

instance GArbitraryList rep => GArbitraryList (M1 i t rep) where
  garbitraryList = fmap M1 <$> garbitraryList

genericArbitraryList :: (Generic a, GArbitrary (Rep a), GArbitraryList (Rep a)) => Gen [a]
genericArbitraryList = fmap to <$> garbitraryList
-}


{- Inspect tree
data Sample = Sample1 Int | Sample2 Int String deriving (Eq,Show,Generic)
λ> :kind! Rep Sample ()
Rep Sample () :: *
= D1
    D1Sample
    (C1 C1_0Sample (S1 NoSelector (Rec0 Int))
     :+: C1
           C1_1Sample
           (S1 NoSelector (Rec0 Int) :*: S1 NoSelector (Rec0 String)))
    ()


:set -XDeriveGeneric
import GHC.Generics
data SumType = SumType1  Int
             | SumType2 String Int
             -- | SumType3  String [Int] Double
             -- | SumType4 String [String] [Int] Double
  deriving (Eq,Generic,Show)
:kind! Rep SumType ()
Rep SumType () :: *
= D1
    D1SumType
    (C1 C1_0SumType (S1 NoSelector (Rec0 Int))
     :+: C1
           C1_1SumType
           (S1 NoSelector (Rec0 String) :*: S1 NoSelector (Rec0 Int)))
    ()
-}
