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
{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators     #-}

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
  , GToADTArbitrarySingleton(..)
  , GToADTArbitrary(..)
  , GArbitrary(..)
  , genericArbitrary

  ) where

import           Data.Typeable

import           GHC.Generics

import           Test.QuickCheck.Arbitrary
import           Test.QuickCheck.Gen


-- $datatypes

-- | ConstructorArbitraryPair holds the construct name as a string and an
-- arbitrary instance of that constructor.
data ConstructorArbitraryPair a = ConstructorArbitraryPair {
  capConstructor :: String
, capArbitrary   :: a
} deriving (Eq,Generic,Read,Show,Typeable)

-- | fmap applies a function to `capArbitrary`
instance Functor ConstructorArbitraryPair where
  fmap f (ConstructorArbitraryPair c a) = ConstructorArbitraryPair c (f a)

instance (Arbitrary a) => Arbitrary (ConstructorArbitraryPair a) where
  arbitrary = ConstructorArbitraryPair <$> arbitrary <*> arbitrary

-- | ADTArbitrarySingleton holds the type name and one ConstructorArbitraryPair.
data ADTArbitrarySingleton a = ADTArbitrarySingleton {
  adtasModuleName :: String
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
data ADTArbitrary a = ADTArbitrary {
  adtModuleName :: String
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
class (Arbitrary a) => ToADTArbitrary a where
  -- {-# MINIMAL toADTArbitrarySingleton, toADTArbitrary #-}
  -- | produce an arbitrary instance of one random constructor
  toADTArbitrarySingleton :: Proxy a -> Gen (ADTArbitrarySingleton a)
  default toADTArbitrarySingleton ::( Arbitrary a
                                    , Generic a
                                    , GToADTArbitrarySingleton (Rep a)
                                    , GToADTArbitrarySingleton (Rep (ADTArbitrarySingleton a)))
                                    => Proxy a
                                    -> Gen (ADTArbitrarySingleton a)
  toADTArbitrarySingleton _ = fmap to <$> gToADTArbitrarySingleton (Proxy :: Proxy (Rep a))

  -- | produce an arbitrary instance for each constructor in type a.
  toADTArbitrary :: Proxy a -> Gen (ADTArbitrary a)
  default toADTArbitrary :: ( Arbitrary a
                            , Generic a
                            , GToADTArbitrary (Rep a)
                            , GToADTArbitrary (Rep (ADTArbitrary a)))
                           => Proxy a
                           -> Gen (ADTArbitrary a)
  toADTArbitrary _ = fmap to <$> gToADTArbitrary (Proxy :: Proxy (Rep a))



-- $generictypeclasses

-- | GToADTArbitrarySingleton creates an arbitrary value and returns the name of the
-- constructor that was used to create it and the type name.

class GToADTArbitrarySingleton rep where
  gToADTArbitrarySingleton :: Proxy rep -> Gen (ADTArbitrarySingleton (rep a))

instance GToADTArbitrarySingleton U1 where
  gToADTArbitrarySingleton _ = pure $ ADTArbitrarySingleton "" "" $ ConstructorArbitraryPair "" U1

instance (GToADTArbitrarySingleton l, GToADTArbitrarySingleton r) => GToADTArbitrarySingleton (l :+: r) where
  gToADTArbitrarySingleton _ = do
    b <- arbitrary
    if b then fmap L1 <$> gToADTArbitrarySingleton (Proxy :: Proxy l)
         else fmap R1 <$> gToADTArbitrarySingleton (Proxy :: Proxy r)

instance (GToADTArbitrarySingleton l, GToADTArbitrarySingleton r) => GToADTArbitrarySingleton (l :*: r) where
  gToADTArbitrarySingleton _ = do
    x <- getArb <$> gToADTArbitrarySingleton (Proxy :: Proxy l)
    y <- getArb <$> gToADTArbitrarySingleton (Proxy :: Proxy r)
    return $ ADTArbitrarySingleton "" "" $ ConstructorArbitraryPair "" (x :*: y)
    where
      getArb = capArbitrary . adtasCAP

instance Arbitrary a => GToADTArbitrarySingleton (K1 i a) where
  gToADTArbitrarySingleton _ = ADTArbitrarySingleton
                           <$> pure ""
                           <*> pure ""
                           <*>  ( ConstructorArbitraryPair
                              <$> pure ""
                              <*> K1 <$> arbitrary
                                )

instance (Constructor c, GToADTArbitrarySingleton rep) => GToADTArbitrarySingleton (M1 C c rep) where
  gToADTArbitrarySingleton _ = ADTArbitrarySingleton
                           <$> pure ""
                           <*> pure ""
                           <*> ( ConstructorArbitraryPair con <$> ac)
    where
      kRep = gToADTArbitrarySingleton (Proxy :: Proxy rep)
      ac   = M1 . capArbitrary . adtasCAP <$> kRep
      con = conName (undefined :: M1 C c rep ())

instance (Datatype t, Typeable t, GToADTArbitrarySingleton rep) => GToADTArbitrarySingleton (M1 D t rep) where
  gToADTArbitrarySingleton _ =  ADTArbitrarySingleton
                             <$> pure m
                             <*> pure t
                             <*>  ( ConstructorArbitraryPair
                                 <$> (capConstructor . adtasCAP <$> kRep)
                                 <*> ac)
    where
      kRep = gToADTArbitrarySingleton (Proxy :: Proxy rep)
      ac   = M1 . capArbitrary . adtasCAP <$> kRep
      m    = moduleName (undefined :: M1 D t rep ())
      t    = datatypeName (undefined :: M1 D t rep ())

instance GToADTArbitrarySingleton rep => GToADTArbitrarySingleton (M1 S t rep) where
  gToADTArbitrarySingleton _ = ADTArbitrarySingleton
                            <$> pure ""
                            <*> pure ""
                            <*>  ( ConstructorArbitraryPair
                                <$> pure ""
                                <*> ac)
    where
      kRep = gToADTArbitrarySingleton (Proxy :: Proxy rep)
      ac   = M1 . capArbitrary . adtasCAP <$> kRep



-- | GToADTArbitrary is a typeclass for generalizing the creation
-- of a list of arbitrary values for each constructor of a type.  It also
-- returns the name of the constructor and the type name for reference and file
-- creation.

class GToADTArbitrary rep where
  gToADTArbitrary :: Proxy rep -> Gen (ADTArbitrary (rep a))

instance GToADTArbitrary U1 where
  gToADTArbitrary _ = pure $ ADTArbitrary "" "" [ConstructorArbitraryPair "" U1]

instance (GToADTArbitrary l, GToADTArbitrary r) => GToADTArbitrary (l :+: r) where
  gToADTArbitrary _ = do
    a <- fmap L1 <$> gToADTArbitrary (Proxy :: Proxy l)
    b <- fmap R1 <$> gToADTArbitrary (Proxy :: Proxy r)
    return $ ADTArbitrary "" "" (adtCAPs a ++ adtCAPs b)

instance (GToADTArbitrarySingleton l, GToADTArbitrary l, GToADTArbitrarySingleton r, GToADTArbitrary r) => GToADTArbitrary (l :*: r) where
  gToADTArbitrary _ = do
    x <- getArb <$> gToADTArbitrarySingleton (Proxy :: Proxy l)
    y <- getArb <$> gToADTArbitrarySingleton (Proxy :: Proxy r)
    return $ ADTArbitrary "" "" [ConstructorArbitraryPair "" (x :*: y)]
    where
      getArb = capArbitrary . adtasCAP

instance Arbitrary a => GToADTArbitrary (K1 i a) where
  gToADTArbitrary _ = ADTArbitrary
                        <$> pure ""
                        <*> pure ""
                        <*> (:[]) <$> genCap
    where
      arb    = arbitrary :: Gen a
      genCap = ConstructorArbitraryPair <$> pure "" <*> (K1 <$> arb)

-- constructor level
instance (Constructor c, GToADTArbitrary rep) => GToADTArbitrary (M1 C c rep) where
  gToADTArbitrary _ = ADTArbitrary <$> pure "" <*> pure "" <*> (:[]) . ConstructorArbitraryPair con <$> ac
    where
      kRep = gToADTArbitrary (Proxy :: Proxy rep)
      ac   = M1 . capArbitrary . head . adtCAPs <$> kRep
      con  = conName (undefined :: M1 C c rep ())

-- type level
instance (Datatype t, GToADTArbitrary rep) => GToADTArbitrary (M1 D t rep) where
  gToADTArbitrary _ =  ADTArbitrary <$> pure m <*> pure t <*> m1caps
    where
      kRep   = gToADTArbitrary (Proxy :: Proxy rep)
      caps   = adtCAPs <$> kRep
      m1caps = (fmap . fmap) M1 <$> caps
      m      = moduleName (undefined :: M1 D t rep ())
      t      = datatypeName (undefined :: M1 D t rep ())

-- selector level
instance GToADTArbitrary rep => GToADTArbitrary (M1 S t rep) where
  gToADTArbitrary _ = ADTArbitrary
                            <$> pure ""
                            <*> pure ""
                            <*> (:[]) <$> (ConstructorArbitraryPair "" <$> ac)
    where
      kRep = gToADTArbitrary (Proxy :: Proxy rep)
      ac   = M1 . capArbitrary . head . adtCAPs <$> kRep


-- | GArbitrary is a typeclass for generalizing the creation of single arbitrary
-- product and sum types. It creates an arbitrary generating function of this
-- style: @TypeName \<$\> arbitrary \<*\> arbitrary@.
--

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
-- > data Fruit = Apple  Int
-- >            | Orange String Int
-- >            | PassionFruit Int String Int
-- >   deriving (Generic, Show)
-- >
-- > -- Product Type, single constructor
-- > data Person = Person {
-- >   name :: String
-- > , age  :: Int
-- > } deriving (Generic, Show)
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
