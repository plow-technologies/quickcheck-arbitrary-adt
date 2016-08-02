{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}

module Test.QuickCheck.Arbitrary.ADTSpec (main, spec) where

import           Control.Monad.IO.Class

import           Control.Lens
import           Data.Maybe          (isJust)
import           GHC.Generics

import           Language.Haskell.TH

import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary.ADT


import           Data.Proxy

-- | A tagless type has constructor which has no parameters. It has U1 in its
-- `GHC.Generics` representation.
data TaglessType = TaglessType
  deriving (Eq,Generic,Show)

instance ToADTArbitrary TaglessType
instance Arbitrary TaglessType where
  arbitrary = genericArbitrary

--instance GToArbitraryConstructorT TaglessType

$(makePrisms ''TaglessType)

-- | A sum type (disjoin union, tagged union) has multiple type constructors
-- that all result in values of different shapes, but the same type. It has
-- `:+:` and `M1 C c rep` in its `GHC.Generics` representation.
data SumType = SumType1 Int
             | SumType2 String Int
             | SumType3  String [Int] Double
             | SumType4 String [String] [Int] Double
  deriving (Eq,Generic,Show)

instance ToADTArbitrary SumType
instance Arbitrary SumType where
  arbitrary = genericArbitrary

$(makePrisms ''SumType)

-- | This type is an example of a sum type that has constructors building sum
-- types.
data SumOfSums = SSBareSumType TaglessType
               | SSSumType SumType
  deriving (Eq,Generic,Show)

instance ToADTArbitrary SumOfSums
instance Arbitrary SumOfSums where
  arbitrary = genericArbitrary

$(makePrisms ''SumOfSums)

-- | A product type has one constructor with one records. It has :*: and `M1 S s rep`
-- in its `GHC.Generics` representation.

data ProductType = ProductType {
  name :: String
, age  :: Int
} deriving (Eq,Generic,Show)

instance ToADTArbitrary ProductType
instance Arbitrary ProductType where
  arbitrary = genericArbitrary


data PolymorphicParameterProductType a = PolymorphicParameterProductType {
  firstItem  :: Int
, secondItem :: a
} deriving (Eq,Show,Generic)

instance ToADTArbitrary (PolymorphicParameterProductType String)
instance Arbitrary (PolymorphicParameterProductType String) where
  arbitrary = genericArbitrary


spec :: Spec
spec =
  describe "QuickCheck Arbitrary ADT: ToADTArbitrary type class" $ do
    it "toADTArbitrary of a tagless type should create an instance of the bare constructor" $ do
      _                    <- generate (toADTArbitrarySingleton (Proxy :: Proxy TaglessType))
      taglessType          <- generate (toADTArbitrary (Proxy :: Proxy TaglessType))
      let taglessTypeArbitraries = _capArbitrary <$> _adtCAPs taglessType
      or (isJust . preview _TaglessType <$> taglessTypeArbitraries) `shouldBe` True

    it "toADTArbitrary of a sum type creates an instance with each constructor" $ do
      _        <- generate (toADTArbitrarySingleton (Proxy :: Proxy SumType))
      sumTypes <- generate (toADTArbitrary (Proxy :: Proxy SumType))
      let sumTypeArbitraries = _capArbitrary <$> _adtCAPs sumTypes

      and
        [ or $ isJust . preview _SumType1 <$> sumTypeArbitraries
        , or $ isJust . preview _SumType2 <$> sumTypeArbitraries
        , or $ isJust . preview _SumType3 <$> sumTypeArbitraries
        , or $ isJust . preview _SumType4 <$> sumTypeArbitraries
        , length sumTypeArbitraries == 4
        ] `shouldBe` True

    it "toADTArbitrary of a sum of sum types creates an instance with each constructor of the top level" $ do
      _         <- generate (toADTArbitrarySingleton (Proxy :: Proxy SumOfSums))
      sumOfSums <- generate (toADTArbitrary (Proxy :: Proxy SumOfSums))
      let sumOfSumsArbitraries = _capArbitrary <$> _adtCAPs sumOfSums
      and
        [ or $ isJust . preview _SSBareSumType <$> sumOfSumsArbitraries
        , or $ isJust . preview _SSSumType <$> sumOfSumsArbitraries
        , length sumOfSumsArbitraries == 2
        ] `shouldBe` True

    it "toADTArbitrary of a product type creates a single instance" $ do
      _           <- generate (toADTArbitrarySingleton (Proxy :: Proxy ProductType))
      productType <- generate (toADTArbitrary (Proxy :: Proxy ProductType))
      let productTypeArbitraries = _capArbitrary <$> _adtCAPs productType

      length productTypeArbitraries `shouldBe` 1

    it "toADTArbitrary of a product type that has a polymorphic parameter should work as well" $ do
      _           <- generate (toADTArbitrarySingleton (Proxy :: Proxy (PolymorphicParameterProductType String)))
      productType <- generate (toADTArbitrary (Proxy :: Proxy (PolymorphicParameterProductType String)))
      let productTypeArbitraries = _capArbitrary <$> _adtCAPs productType

      length productTypeArbitraries `shouldBe` 1


main :: IO ()
main = hspec spec
