{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE TemplateHaskell  #-}

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

$(makePrisms ''ProductType)

spec :: Spec
spec =
  describe "QuickCheck Arbitrary ADT: ToArbitraryConstructor type class" $ do
    it "toArbitraryConstructorList of a tagless type should create an instance of the bare constructor" $ do
      --taglessType <- fmap snd <$> generate (toArbitraryConstructorList :: Gen [(String, TaglessType)])
      taglessTypeSingleton <- generate (toADTArbitrarySingleton (Proxy :: Proxy TaglessType))
      taglessType          <- generate (toADTArbitrary (Proxy :: Proxy TaglessType))
      liftIO $ print  taglessType
      --or (isJust . preview _TaglessType <$> taglessType) `shouldBe` True
    {-
    it "toArbitraryConstructorList of a sum type creates an instance with each constructor" $ do
      -- remove the constructor tags
      sumTypes <- fmap snd <$> generate (toArbitraryConstructorList :: Gen [(String,SumType)])
      tss <- generate (genericToADTArbitrary (Proxy :: Proxy SumType))
      liftIO $ print tss
      and
        [ or $ isJust . preview _SumType1 <$> sumTypes
        , or $ isJust . preview _SumType2 <$> sumTypes
        , or $ isJust . preview _SumType3 <$> sumTypes
        , or $ isJust . preview _SumType4 <$> sumTypes
        , length sumTypes == 4
        ] `shouldBe` True

    it "toArbitraryConstructorList of a sum of sum types creates an instance with each constructor of the top level" $ do
      sumOfSums <- fmap snd <$> generate (toArbitraryConstructorList :: Gen [(String, SumOfSums)])
      tss <- generate (genericToADTArbitrary (Proxy :: Proxy SumOfSums))
      liftIO $ print tss
      and
        [ or $ isJust . preview _SSBareSumType <$> sumOfSums
        , or $ isJust . preview _SSSumType <$> sumOfSums
        , length sumOfSums == 2
        ] `shouldBe` True


    it "toArbitraryConstructorList of a product type creates a single instance" $ do
      productTypes <- generate (toArbitraryConstructorList :: Gen [(String,ProductType)])
      tt <- generate (genericToADTArbitrarySingleton (Proxy :: Proxy ProductType))
      liftIO $ print tt
      tss <- generate (genericToADTArbitrary (Proxy :: Proxy ProductType))
      liftIO $ print tss
      length productTypes `shouldBe` 1
    -}
main :: IO ()
main = hspec spec
