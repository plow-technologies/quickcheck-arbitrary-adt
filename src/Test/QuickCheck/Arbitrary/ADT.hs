{-|
Module      : Test.QuickCheck.Arbitrary.ADT
Description : Generate arbitrary values for all constructors
Copyright   : Plow Technologies LLC
License     : BSD3
Maintainer  : mchaver@gmail.com
Stability   : Beta

Type classes to assist random generation of values for various types of
abstract data types.
-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Test.QuickCheck.Arbitrary.ADT (
  -- * How to use this library
  -- $use

  -- * Data types
  -- $datatypes
    ConstructorArbitraryPair(..)
  , ADTArbitrarySingleton(..)
  , ADTArbitrary(..)

  -- * Type classes
  -- $typeclasses
  , ToADTArbitrary(..)

  -- * Generic type classes
  -- $generictypeclasses
  , toADTArbitraryForConstructors
  , ToADTArbitraryHelpers (..)
  , GArbitrary(..)
  , genericArbitrary

  ) where

-- base
import Data.Typeable
import Data.Maybe (catMaybes)
import GHC.Generics

-- QuickCheck
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

-- $datatypes

-- | ConstructorArbitraryPair holds the construct name as a string and an
-- arbitrary instance of that constructor.
data ConstructorArbitraryPair a =
  ConstructorArbitraryPair
    { capConstructor :: String
    , capArbitrary   :: a
    } deriving (Eq,Generic,Read,Show,Typeable)

-- | fmap applies a function to `capArbitrary`
instance Functor ConstructorArbitraryPair where
  fmap f (ConstructorArbitraryPair c a) = ConstructorArbitraryPair c (f a)

instance (Arbitrary a) => Arbitrary (ConstructorArbitraryPair a) where
  arbitrary = ConstructorArbitraryPair <$> arbitrary <*> arbitrary

-- | ADTArbitrarySingleton holds the type name and one ConstructorArbitraryPair.
data ADTArbitrarySingleton a =
  ADTArbitrarySingleton
    { adtasModuleName :: String
    , adtasTypeName   :: String
    , adtasCAP        :: ConstructorArbitraryPair a
    } deriving (Eq,Generic,Read,Show,Typeable)

-- | fmap applies a function to the ConstructorArbitraryPair in adtasCAP.
instance Functor ADTArbitrarySingleton where
  fmap f (ADTArbitrarySingleton m t c) = ADTArbitrarySingleton m t (f <$> c)

instance (Arbitrary a) => Arbitrary (ADTArbitrarySingleton a) where
  arbitrary = ADTArbitrarySingleton <$> arbitrary <*> arbitrary <*> arbitrary

-- | ADTArbitrary holds the type name and a ConstructorArbitraryPair
-- for each constructor.
data ADTArbitrary a =
  ADTArbitrary
    { adtModuleName :: String
    , adtTypeName   :: String
    , adtCAPs       :: [ConstructorArbitraryPair a]
    } deriving (Eq,Generic,Read,Show,Typeable)

-- | fmap applies a function to each ConstructorArbitraryPair in adtCAPs.
instance Functor ADTArbitrary where
  fmap f (ADTArbitrary m t cs) = ADTArbitrary m t (fmap f <$> cs)

instance (Arbitrary a) => Arbitrary (ADTArbitrary a) where
  arbitrary = ADTArbitrary <$> arbitrary <*> arbitrary <*> arbitrary


-- $typeclasses

-- | ToADTArbitrary generalizes the production of arbitrary values for Sum types.
-- and Product types.
class ToADTArbitrary a where
  -- {-# MINIMAL toADTArbitrarySingleton, toADTArbitrary #-}
  -- | produce an arbitrary instance of one random constructor
  toADTArbitrarySingleton :: Proxy a -> Gen (ADTArbitrarySingleton a)
  default toADTArbitrarySingleton :: (Generic a, ToADTArbitraryHelpers (Rep a), Arbitrary a) => Proxy a -> Gen (ADTArbitrarySingleton a)
  toADTArbitrarySingleton p =
    ADTArbitrarySingleton
      <$> pure m
      <*> pure t
      <*> ((\a -> ConstructorArbitraryPair (constructorName a) a) <$> arbitrary)
    where
      (m, t) = moduleAndDataName p

  -- | produce an arbitrary instance for each constructor in type a.
  toADTArbitrary :: Proxy a -> Gen (ADTArbitrary a)
  default toADTArbitrary :: (Generic a, ToADTArbitraryHelpers (Rep a), Arbitrary a) => Proxy a -> Gen (ADTArbitrary a)
  toADTArbitrary p =
    toADTArbitraryForConstructors (allConstructors p) p


-- $generictypeclasses

class ToADTArbitraryHelpers f where
  moduleAndDataName' :: f a -> (String, String)
  constructorName' :: f a -> String
  allConstructors' :: f a -> [String]

instance (Datatype t, ToADTArbitraryHelpers f) => ToADTArbitraryHelpers (M1 D t f) where
  moduleAndDataName' d = (moduleName d, datatypeName d)
  constructorName' (M1 x) = constructorName' x
  allConstructors' _ = allConstructors' (undefined :: f x)

instance (ToADTArbitraryHelpers x, ToADTArbitraryHelpers y) => ToADTArbitraryHelpers (x :+: y) where
  moduleAndDataName' = undefined
  constructorName' (L1 l) = constructorName' l
  constructorName' (R1 r) = constructorName' r
  allConstructors' _ = allConstructors' (undefined :: x a) ++ allConstructors' (undefined :: y b)

instance Constructor c => ToADTArbitraryHelpers (C1 c f) where
  moduleAndDataName' = undefined
  constructorName' x = conName x
  allConstructors' x = [conName x]

moduleAndDataName :: forall a. (Generic a, ToADTArbitraryHelpers (Rep a)) => Proxy a -> (String, String)
moduleAndDataName _ = moduleAndDataName' $ from (undefined :: a)

constructorName :: (ToADTArbitraryHelpers (Rep a), Generic a) => a -> String
constructorName = constructorName' . from

allConstructors :: forall a. (Generic a, ToADTArbitraryHelpers (Rep a)) => Proxy a -> [String]
allConstructors _ = allConstructors' $ from (undefined :: a)



suchThatLimitTries :: Int -> Gen a -> (a -> Bool) -> Gen (Maybe a)
gen `suchThatLimitTries n` p = try n
 where
  try m
    | m > 0 = do
        x <- gen
        if p x then return (Just x) else try (m-1)
    | otherwise = return Nothing


toADTArbitraryForConstructors :: (Generic a, ToADTArbitraryHelpers (Rep a), Arbitrary a) => [String] -> Proxy a -> Gen (ADTArbitrary a)
toADTArbitraryForConstructors constrs p =
  ADTArbitrary
    <$> pure m
    <*> pure t
    <*> (catMaybes <$> sequence
      [((ConstructorArbitraryPair c) <$>) <$> arbitrary `suchThatLimitTries 500` ((== c) . constructorName) | c <- constrs])
  where
    (m, t) = moduleAndDataName p

-- | GArbitrary is a typeclass for generalizing the creation of single arbitrary
-- product and sum types. It creates an arbitrary generating function of this
-- style: @TypeName \<$\> arbitrary \<*\> arbitrary@.

class GArbitrary rep where
  gArbitrary :: Gen (rep a)

instance GArbitrary U1 where
  gArbitrary = pure U1

instance (GArbitrary l, GArbitrary r) => GArbitrary (l :+: r) where
  gArbitrary = do
    b <- arbitrary
    if b then L1 <$> gArbitrary
         else R1 <$> gArbitrary

instance (GArbitrary l, GArbitrary r) => GArbitrary (l :*: r) where
  gArbitrary = (:*:) <$> gArbitrary <*> gArbitrary

instance Arbitrary a => GArbitrary (K1 i a) where
  gArbitrary = K1 <$> arbitrary

instance GArbitrary rep => GArbitrary (M1 i t rep) where
  gArbitrary = M1 <$> gArbitrary

-- | Create a arbitrary generator for a specified a type in a naive way. Please
-- be careful when using this function, particularly for recursive types.
genericArbitrary :: (Generic a, GArbitrary (Rep a)) => Gen a
genericArbitrary = to <$> gArbitrary

-- $use
--
-- How to use `ToADTArbitrary` with Generic.
--
-- > {-# LANGUAGE DeriveGeneric #-}
-- >
-- > import Data.Proxy
-- > import GHC.Generics
-- > import Test.QuickCheck
-- > import Test.QuickCheck.Arbitrary.ADT
-- >
-- > -- Sum Type, multiple constructors with parameters
-- > data Fruit
-- >   = Apple Int
-- >   | Orange String Int
-- >   | PassionFruit Int String Int
-- >   deriving (Generic, Show)
-- >
-- > -- Product Type, single constructor
-- > data Person =
-- >   Person
-- >     { name :: String
-- >     , age  :: Int
-- >     } deriving (Generic, Show)
--
-- Any type that implements `ToADTArbitrary` must also implement `Arbitrary`.
-- These examples all require that the data type is an instance of `Generic`.
--
-- @
-- instance Arbitrary Fruit where
--   arbitrary = `genericArbitrary`
--
-- instance `ToADTArbitrary` Fruit
--
-- instance Arbitrary Person where
--   arbitrary = `genericArbitrary`
--
-- instance `ToADTArbitrary` Person
-- @
--
-- Now we can use `toADTArbitrarySingleton` to produce an arbitrary value of
-- one random constructor along with some metadata. `toADTArbitrary` will
-- produce an arbitrary value for each constructor and return it along with
-- a String of the constructor name.
--
-- @
-- λ> generate (toADTArbitrarySingleton (Proxy :: Proxy Fruit))
-- ADTArbitrarySingleton {
--   adtasModuleName = \"Ghci1\"
-- , adtasTypeName = \"Fruit\"
-- , adtasCAP = ConstructorArbitraryPair {
--     capConstructor = \"Apple\", capArbitrary = Apple 30}}
--
-- λ> generate (toADTArbitrary (Proxy :: Proxy Fruit))
-- ADTArbitrary {
--   adtModuleName = \"Ghci1\"
-- , adtTypeName = \"Fruit\"
-- , adtCAPs = [
--     ConstructorArbitraryPair {
--         capConstructor = \"Apple\"
--       , capArbitrary = Apple 17}
--   , ConstructorArbitraryPair {
--         capConstructor = \"Orange\"
--       , capArbitrary = Orange \"abcdef\" 18}
--   , ConstructorArbitraryPair {
--         capConstructor = \"PassionFruit\"
--       , capArbitrary = PassionFruit 16 "datadata" 6}]}
--
-- λ> generate (toADTArbitrarySingleton (Proxy :: Proxy Person))
-- ADTArbitrarySingleton {
--   adtasModuleName = \"Ghci1\"
-- , adtasTypeName = \"Person\"
-- , adtasCAP = ConstructorArbitraryPair {capConstructor = \"Person\", capArbitrary = Person {name = "John Doe", age = 30}}}
--
-- λ> generate (toADTArbitrary (Proxy :: Proxy Person))
-- ADTArbitrary {
--   adtModuleName = \"Ghci1\"
-- , adtTypeName = \"Person\"
-- , adtCAPs = [ConstructorArbitraryPair {capConstructor = \"Person\", capArbitrary = Person {name = "Jane Doe", age = 15}}]}
-- @
