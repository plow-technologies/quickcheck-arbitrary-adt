{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}

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
