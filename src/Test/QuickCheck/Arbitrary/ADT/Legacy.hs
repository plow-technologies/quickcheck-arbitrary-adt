{-|
Module      : Test.QuickCheck.Arbitrary.ADT.Legacy
Description : Legacy type class for random generation of abstract data types.
              It is maintained for backwards compatability and as an optional
              choice, but will not be developed further.
Copyright   : (c) Plow Technologies LLC, 2016-2019
License     : BSD3
Maintainer  : mchaver@gmail.com

Type classes for random generation of values for each constructor of an abstract
data type.

'ToADTArbitrary' requires a type to have a 'Generic' instance and any types used
in its constructors have to have an 'Arbitrary' instance.

The generic instance of 'ToADTArbitrary' should be considered defective.
It does not use the 'Arbitrary' instance of a type but rather it builds a
generic 'Arbitrary' instance using the types of its records/fields, whereas
what the library user would expect is that it uses the 'Arbitrary' of the type
itself.

For that reason, the code in this file is now longer further developed, but will
be continuallymaintained for backwards compatibility.
-}

{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Test.QuickCheck.Arbitrary.ADT.Legacy
  (
  -- * How to use this library
  -- $legacy_use

  -- * Data types
  -- $legacy_datatypes
    ADTArbitrarySingleton(..)

  -- * Type classes
  -- $legacy_typeclasses
  , ToADTArbitrary(..)

  -- * Generic type classes
  -- $legacy_generictypeclasses
  , GToADTArbitrarySingleton(..)
  , GToADTArbitrary(..)
  , GArbitrary(..)
  , genericArbitrary
    
  ) where

-- base
import Data.Typeable (Typeable, Proxy(Proxy))
import GHC.Generics  (Generic, Constructor, Datatype, Rep, M1(M1), K1(K1),
                      U1(U1), C, D, S, (:+:)(L1, R1), (:*:)((:*:)), conName,
                      datatypeName, moduleName, to)

-- QuickCheck
import Test.QuickCheck (Arbitrary, Gen, arbitrary)
import Test.QuickCheck.Arbitrary.ADT.Types (ConstructorArbitraryPair(..), ADTArbitrary(..))

-- $legacy_datatypes

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

-- $legacy_typeclasses

-- | ToADTArbitrary generalizes the production of arbitrary values for Sum types.
-- and Product types.
class ToADTArbitrary a where
  -- {-# MINIMAL toADTArbitrarySingleton, toADTArbitrary #-}
  -- | produce an arbitrary instance of one random constructor
  toADTArbitrarySingleton :: Proxy a -> Gen (ADTArbitrarySingleton a)
  default toADTArbitrarySingleton ::
    ( Generic a
    , GToADTArbitrarySingleton (Rep a)
    )
    => Proxy a
    -> Gen (ADTArbitrarySingleton a)
  toADTArbitrarySingleton _ = fmap to <$> gToADTArbitrarySingleton (Proxy :: Proxy (Rep a))

  -- | produce an arbitrary instance for each constructor in type a.
  toADTArbitrary :: Proxy a -> Gen (ADTArbitrary a)
  default toADTArbitrary ::
    ( Generic a
    , GToADTArbitrary (Rep a)
    )
    => Proxy a
    -> Gen (ADTArbitrary a)
  toADTArbitrary _ = fmap to <$> gToADTArbitrary (Proxy :: Proxy (Rep a))



-- $legacy_generictypeclasses

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
  gToADTArbitrarySingleton _ =
    ADTArbitrarySingleton
      <$> pure ""
      <*> pure ""
      <*> (ConstructorArbitraryPair
            <$> pure ""
            <*> K1 <$> arbitrary)

instance (Constructor c, GToADTArbitrarySingleton rep) => GToADTArbitrarySingleton (M1 C c rep) where
  gToADTArbitrarySingleton _ =
    ADTArbitrarySingleton
      <$> pure ""
      <*> pure ""
      <*> (ConstructorArbitraryPair con <$> ac)
    where
      kRep = gToADTArbitrarySingleton (Proxy :: Proxy rep)
      ac   = M1 . capArbitrary . adtasCAP <$> kRep
      con = conName (undefined :: M1 C c rep ())

instance (Datatype t, Typeable t, GToADTArbitrarySingleton rep) => GToADTArbitrarySingleton (M1 D t rep) where
  gToADTArbitrarySingleton _ =
    ADTArbitrarySingleton
      <$> pure m
      <*> pure t
      <*> (ConstructorArbitraryPair
            <$> (capConstructor . adtasCAP <$> kRep)
            <*> ac)
    where
      kRep = gToADTArbitrarySingleton (Proxy :: Proxy rep)
      ac   = M1 . capArbitrary . adtasCAP <$> kRep
      m    = moduleName (undefined :: M1 D t rep ())
      t    = datatypeName (undefined :: M1 D t rep ())

instance GToADTArbitrarySingleton rep => GToADTArbitrarySingleton (M1 S t rep) where
  gToADTArbitrarySingleton _ =
    ADTArbitrarySingleton
      <$> pure ""
      <*> pure ""
      <*> (ConstructorArbitraryPair
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

instance (GToADTArbitrarySingleton l, GToADTArbitrarySingleton r) => GToADTArbitrary (l :*: r) where
  gToADTArbitrary _ = do
    x <- getArb <$> gToADTArbitrarySingleton (Proxy :: Proxy l)
    y <- getArb <$> gToADTArbitrarySingleton (Proxy :: Proxy r)
    return $ ADTArbitrary "" "" [ConstructorArbitraryPair "" (x :*: y)]
    where
      getArb = capArbitrary . adtasCAP

instance Arbitrary a => GToADTArbitrary (K1 i a) where
  gToADTArbitrary _ =
    ADTArbitrary
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
  gToADTArbitrary _ =
    ADTArbitrary
      <$> pure ""
      <*> pure ""
      <*> (:[]) <$> (ConstructorArbitraryPair "" <$> ac)
    where
      kRep = gToADTArbitrary (Proxy :: Proxy rep)
      ac   = M1 . capArbitrary . head . adtCAPs <$> kRep


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

-- $legacy_use
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
