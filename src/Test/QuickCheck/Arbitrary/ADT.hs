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

class GArbitraryList rep where
  garbitraryList :: Gen [(String, rep a)]

instance (GArbitraryList l, GArbitraryList r) => GArbitraryList (l :+: r) where
  garbitraryList = (++) <$> ((fmap . fmap) L1 <$> garbitraryList) <*> ((fmap . fmap) R1 <$> garbitraryList)

--instance (GArbitraryList l, GArbitrary l, GArbitraryList r, GArbitrary r, GArbitrary []) => GArbitraryList (l :*: r) where
  --garbitraryList = fmap pure <$> garbitrary
--  garbitraryList = (:*:) <$> pure [("",)] <*> pure []

--instance Arbitrary a => GArbitraryList (K1 i a) where
--  garbitraryList = K1 <$> arbitrary --  fmap (:[]) . K1 <$> arbitrary

instance (Constructor c) => GArbitraryList (M1 C c U1) where
  garbitraryList = (fmap . fmap) M1 <$> pure [(con , U1)]
    where
      con = conName (undefined :: M1 C c U1 ())
-- (selName (undefined :: M1 C x U1 ())
--instance (Selector x , GArbitraryList rep) => GArbitraryList (M1 C x rep) where

instance (Constructor c, Arbitrary a) => GArbitraryList (M1 C c (K1 z a)) where
  garbitraryList = (fmap . fmap) M1 <$> (:[]) <$> ((,) <$> pure con <*> (K1 <$> arbitrary))
    where
      con = conName (undefined :: M1 C c (K1 z a) ())

--instance GArbitraryList f => GArbitraryList (M1 D x f) where
--  garbitraryList = garbitraryList

instance (Constructor c, Selector s, Arbitrary a) =>  GArbitraryList (M1 C c (M1 S s (K1 z a))) where
  garbitraryList = (fmap . fmap) M1 <$> (:[]) <$> ((,) <$> pure con <*> (M1 . K1 <$> arbitrary))
      where
        --con = conName (undefined :: M1 C c () ())
        con = conName (undefined :: M1 C c (M1 S s (K1 z a)) ())

--instance Arbitrary a => GArbitraryList (K1 i a) where
--  garbitraryList = (:[]) . ((,) "") . K1 <$> arbitrary


instance GArbitraryList rep => GArbitraryList (M1 D t rep) where
  garbitraryList = (fmap . fmap) M1 <$> garbitraryList

{-
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
