{-|
Module      : Test.QuickCheck.Arbitrary.ADT
Description : Generate arbitrary values for all constructors
Copyright   : Plow Technologies LLC
License     : BSD3
Maintainer  : mchaver@gmail.com

Type classes to assist random generation of values for various types of
abstract data types.
-}

{-# LANGUAGE ScopedTypeVariables #-}

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

  -- * Arbitrary Abstract Data Type functions
  , getConstructors
  , arbitraryWithConstructor
  , arbitraryForConstructors
  , arbitraryAdt
  ) where

-- re-export
import Test.QuickCheck.Arbitrary.ADT.Types  (ConstructorArbitraryPair(..), ADTArbitrary(..))
import Test.QuickCheck.Arbitrary.ADT.Legacy
  (ADTArbitrarySingleton(..), ToADTArbitrary(..), GToADTArbitrarySingleton(..),
   GToADTArbitrary(..), GArbitrary(..), genericArbitrary)

import Control.Monad   (forM)
import Data.Data       (Data, Constr, Proxy, dataTypeConstrs, dataTypeOf,
                        showConstr, toConstr)
import Data.Typeable   (Typeable, tyConModule, tyConName, typeRepTyCon, typeRep)
import Test.QuickCheck (Arbitrary, Gen, arbitrary)

-- | Get a list of all of the constructors of a type.
getConstructors :: forall a. Data a => Proxy a -> [Constr]
getConstructors _proxy = dataTypeConstrs $ dataTypeOf (undefined :: a)

-- | Generate an arbitrary value for a given type constructor. It will loop
-- idenfinitely until a type made with the provided constructor is found.
-- It maybe slow on large sum types. This function is considered dangerous
-- becausee you can provide a 'Constr' that does not much
-- type 'a' and it will give unexpected results. You are responsible for making
-- sure that 'Constr' is a constructor of type 'a'.
arbitraryWithConstructor :: (Arbitrary a, Data a) => Constr -> Gen a
arbitraryWithConstructor constr = do
  a <- arbitrary
  if constr == toConstr a
    then pure a
    else arbitraryWithConstructor constr

-- | Produce a list of arbitrary values for each constructor of a type.
-- It uses  'arbitraryWithConstructor' internally in a safe manner. The user
-- does not  need to worry about breaking it.
arbitraryForConstructors :: (Arbitrary a, Data a) => Proxy a -> Gen [a]
arbitraryForConstructors proxy = do
  let constructors = getConstructors proxy
  mapM arbitraryWithConstructor constructors

-- | Generate an arbitrary value for each type in a constructor. This includes
-- the module name, the type name, and a list of the constructors as 'String's
-- paired with their arbitrarily generated value. It uses
-- 'arbitraryWithConstructor' internally in a safe manner. The user does not
-- need to worry about breaking it.
arbitraryAdt :: (Arbitrary a, Data a, Typeable a) => Proxy a -> Gen (ADTArbitrary a)
arbitraryAdt proxy = do
  let constructors = getConstructors proxy
  let t = typeRepTyCon . typeRep $ proxy
  pairs <- forM constructors
           (\x -> do
               a <- arbitraryWithConstructor x
               pure $ ConstructorArbitraryPair (showConstr x) a
           )
  pure $ ADTArbitrary (tyConModule t) (tyConName t) pairs
