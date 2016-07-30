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


data SumType = SumType1  Int
             -- | SumType2 String Int
             -- | SumType3  String [Int] Double
             -- | SumType4 String [String] [Int] Double
  deriving (Eq,Generic,Show)

instance Arbitrary SumType where
  arbitrary = genericArbitrary

$(makePrisms ''SumType)

data ProductType = ProductType {
  name :: String
, age  :: Int
} deriving (Eq,Generic,Show)

--instance Arbitrary ProductType where
--  arbitrary = genericArbitrary

--instance GArbitrary []
--instance GArbitraryList []


spec :: Spec
spec =
  describe "QuickCheck Arbitrary ADT" $ do
    it "genericArbitraryList of a sum type creates an instance with each constructor" $ do
      sumTypes <- generate (genericArbitraryList :: Gen [(String,SumType)])
      {-
      and
        [ or $ isJust . preview _SumType1 <$> sumTypes
        , or $ isJust . preview _SumType2 <$> sumTypes
        , or $ isJust . preview _SumType3 <$> sumTypes
        , or $ isJust . preview _SumType4 <$> sumTypes
        , length sumTypes == 4
        ] `shouldBe` True
      -}
      liftIO $ print sumTypes
      False `shouldBe` True
    {-
    it "genericArbitraryList of a product type creates a single instance" $ do

      productTypes <- generate (genericArbitraryList :: Gen [(String,ProductType)])
      {-
      length productTypes `shouldBe` 1
      -}
      liftIO $ print productTypes
      True `shouldBe` False
    -}
main :: IO ()
main = hspec spec
