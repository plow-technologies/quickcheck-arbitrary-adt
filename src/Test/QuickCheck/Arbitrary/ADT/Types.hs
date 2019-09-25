{-|
Module      : Test.QuickCheck.Arbitrary.ADT.Types
Description : Generate arbitrary values for all constructors
Copyright   : Plow Technologies LLC
License     : BSD3
Maintainer  : mchaver@gmail.com

-}

{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Test.QuickCheck.Arbitrary.ADT.Types
  ( ConstructorArbitraryPair(..)
  , ADTArbitrary(..)
  ) where

import Data.Data       (Data)
import Data.Typeable   (Typeable)
import GHC.Generics    (Generic)
import Test.QuickCheck (Arbitrary, arbitrary)

-- $datatypes

-- | ConstructorArbitraryPair holds the construct name as a string and an
-- arbitrary instance made with that constructor.
data ConstructorArbitraryPair a =
  ConstructorArbitraryPair
    { capConstructor :: String
    , capArbitrary   :: a
    } deriving (Data,Eq,Generic,Ord,Read,Show,Typeable)

instance Functor ConstructorArbitraryPair where
  fmap f (ConstructorArbitraryPair c a) = ConstructorArbitraryPair c (f a)

instance (Arbitrary a) => Arbitrary (ConstructorArbitraryPair a) where
  arbitrary = ConstructorArbitraryPair <$> arbitrary <*> arbitrary

-- | ADTArbitrary holds the module name, type name and a
-- ConstructorArbitraryPair  for each constructor of a type.
data ADTArbitrary a =
  ADTArbitrary
    { adtModuleName :: String
    , adtTypeName   :: String
    , adtCAPs       :: [ConstructorArbitraryPair a]
    } deriving (Data,Eq,Generic,Ord,Read,Show,Typeable)

instance Functor ADTArbitrary where
  fmap f (ADTArbitrary m t cs) = ADTArbitrary m t (fmap f <$> cs)

instance (Arbitrary a) => Arbitrary (ADTArbitrary a) where
  arbitrary = ADTArbitrary <$> arbitrary <*> arbitrary <*> arbitrary
