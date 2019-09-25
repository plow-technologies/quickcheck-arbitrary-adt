{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}

module Test.QuickCheck.Arbitrary.ADTSpec
  ( spec
  ) where

-- base
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import           Data.Data       (Data)
import           Data.Maybe      (isJust)
import           Data.Proxy      (Proxy(Proxy))
import           GHC.Generics    (Generic)

-- lens
import           Control.Lens    (makePrisms, preview)

-- testing
import           Test.QuickCheck (Arbitrary, arbitrary, choose, generate, oneof, vector)
import           Test.Hspec      (Spec, describe, it, shouldBe)
import           Test.QuickCheck.Arbitrary.ADT (arbitraryAdt, adtCAPs, capArbitrary)

-- =============================================================================
-- Test types 
-- =============================================================================

-- =====================================
-- newtype
-- =====================================

newtype MyString = MyString ByteString
  deriving (Eq,Data,Generic,Show)

instance Arbitrary MyString where
  arbitrary = MyString . B.pack <$> arbitrary

-- =====================================
-- sum type
-- =====================================

-- | A sum type (disjoin union, tagged union) has multiple type constructors
-- that all result in values of different shapes, but the same type. It has
-- `:+:` and `M1 C c rep` in its `GHC.Generics` representation.
data SumType
  = SumType1 Int
  | SumType2 String Int
  | SumType3 String [Int] Double
  | SumType4 String [String] [Int] Double
  deriving (Eq,Data,Generic,Show)

instance Arbitrary SumType where
  arbitrary = do
    k <- choose (0,2)
    l <- choose (0,2)
    oneof
      [ SumType1 <$> arbitrary
      , SumType2 <$> arbitrary <*> arbitrary
      , SumType3 <$> arbitrary <*> vector k <*> arbitrary
      , SumType4 <$> arbitrary <*> vector k <*> vector l <*> arbitrary
      ]

$(makePrisms ''SumType)

-- =============================================================================
-- spec
-- =============================================================================

spec :: Spec
spec = do
  describe "arbitraryAdt" $ do
    it "newtype" $ do
      myString <- generate (arbitraryAdt (Proxy :: Proxy MyString))
      let myStringArbitraries = capArbitrary <$> adtCAPs myString
      length myStringArbitraries `shouldBe` 1

    it "sumtype" $ do
      sumTypes <- generate (arbitraryAdt (Proxy :: Proxy SumType))
      let sumTypeArbitraries = capArbitrary <$> adtCAPs sumTypes
        
      and
        [ or $ isJust . preview _SumType1 <$> sumTypeArbitraries
        , or $ isJust . preview _SumType2 <$> sumTypeArbitraries
        , or $ isJust . preview _SumType3 <$> sumTypeArbitraries
        , or $ isJust . preview _SumType4 <$> sumTypeArbitraries
        , length sumTypeArbitraries == 4
        ] `shouldBe` True
