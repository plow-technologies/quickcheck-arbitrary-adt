{-|
Module      : Test.QuickCheck.Arbitrary.Stochastic.ADT
Description : Generate arbitrary values for all constructors
Copyright   : Plow Technologies LLC
License     : BSD3
Maintainer  : mchaver@gmail.com
Stability   : Beta

-}

{-# LANGUAGE ScopedTypeVariables #-}

module Test.QuickCheck.Arbitrary.Stochastic.ADT where

import Control.Monad   (forM)
import Data.Data       (Data, Constr, Proxy, dataTypeConstrs, dataTypeOf,
                        showConstr, toConstr)
import Data.Typeable   (Typeable, tyConModule, tyConName, typeRepTyCon, typeRep)
import Test.QuickCheck (Arbitrary, Gen, arbitrary)

import Test.QuickCheck.Arbitrary.ADT (ADTArbitrary(..), ConstructorArbitraryPair(..))


getConstructors :: forall a. Data a => Proxy a -> [Constr]
getConstructors _proxy = dataTypeConstrs $ dataTypeOf (undefined :: a)

arbitraryAdt :: (Arbitrary a, Data a) => Proxy a -> Gen [a]
arbitraryAdt proxy = do
  let constructors = getConstructors proxy
  mapM arbitraryConstructor constructors

arbitraryConstructor :: (Arbitrary a, Data a) => Constr -> Gen a
arbitraryConstructor constr = do
  a <- arbitrary
  if constr == toConstr a
    then pure a
    else arbitraryConstructor constr


arbitraryAdt' :: (Arbitrary a, Data a, Typeable a) => Proxy a -> Gen (ADTArbitrary a)
arbitraryAdt' proxy = do
  let constructors = getConstructors proxy
  let t = typeRepTyCon . typeRep $ proxy
  pairs <- forM constructors
           (\x -> do
               a <- arbitraryConstructor x
               pure $ ConstructorArbitraryPair (showConstr x) a
           )
  pure $ ADTArbitrary (tyConModule t) (tyConName t) pairs

-- toADTArbitrary :: (Arbitrary a, Data a) => Proxy a -> Gen (ADTArbitrary a)
-- toADTArbitrary = ADTArbitrary <$> 
-- generate $ arbitraryAdt (Proxy :: Proxy (Maybe Int))
-- generate $ arbitraryAdt (Proxy @Maybe Int)
{-
data ConstructorArbitraryPair a =
  ConstructorArbitraryPair
    { capConstructor :: String
    , capArbitrary   :: a
    } deriving (Eq,Generic,Read,Show,Typeable)

-}
