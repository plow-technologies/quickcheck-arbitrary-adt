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
  deriving (Data,Eq,Show)

instance Arbitrary MyString where
  arbitrary = MyString . B.pack <$> arbitrary

-- =====================================
-- unit
-- =====================================

data Unit = Unit
  deriving (Data,Eq,Show)

instance Arbitrary Unit where
  arbitrary = pure Unit

$(makePrisms ''Unit)

-- =====================================
-- sum type
-- =====================================

-- | A sum type (disjoin union, tagged union) has multiple type constructors
-- that all result in values of different shapes, but the same type.
data SumType
  = SumType1 Int
  | SumType2 String Int
  | SumType3 String [Int] Double
  | SumType4 String [String] [Int] Double
  deriving (Data,Eq,Show)

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

-- =====================================
-- sum of sums type
-- =====================================

-- | This type is an example of a sum type that has constructors building sum
-- types.
data SumOfSums
  = SSBareSumType Unit
  | SSSumType SumType
  deriving (Data,Eq,Show)

instance Arbitrary SumOfSums where
  arbitrary =
    oneof
      [ pure $ SSBareSumType Unit
      , SSSumType <$> arbitrary
      ]

$(makePrisms ''SumOfSums)

-- =====================================
-- product type
-- =====================================

-- | A product type has one constructor with named fields.
data ProductType =
  ProductType
    { name :: String
    , age  :: Int
    } deriving (Data,Eq,Show)

instance Arbitrary ProductType where
  arbitrary = ProductType <$> arbitrary <*> arbitrary

-- =====================================
-- product type with polymorphic param
-- =====================================

data PolymorphicParameterProductType a =
  PolymorphicParameterProductType
    { firstItem  :: Int
    , secondItem :: a
    } deriving (Data,Eq,Show)

instance (Arbitrary a) => Arbitrary (PolymorphicParameterProductType a) where
  arbitrary = PolymorphicParameterProductType <$> arbitrary <*> arbitrary

-- =============================================================================
-- spec
-- =============================================================================

spec :: Spec
spec = do
  describe "arbitraryAdt" $ do
    it "unit" $ do
      unit          <- generate (arbitraryAdt (Proxy :: Proxy Unit))
      let unitArbitraries = capArbitrary <$> adtCAPs unit
      or (isJust . preview _Unit <$> unitArbitraries) `shouldBe` True
    
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

    it "sum of sum types" $ do
      sumOfSums <- generate (arbitraryAdt (Proxy :: Proxy SumOfSums))
      let sumOfSumsArbitraries = capArbitrary <$> adtCAPs sumOfSums
      and
        [ or $ isJust . preview _SSBareSumType <$> sumOfSumsArbitraries
        , or $ isJust . preview _SSSumType <$> sumOfSumsArbitraries
        , length sumOfSumsArbitraries == 2
        ] `shouldBe` True

    it "product type" $ do
      productType <- generate (arbitraryAdt (Proxy :: Proxy ProductType))
      let productTypeArbitraries = capArbitrary <$> adtCAPs productType

      length productTypeArbitraries `shouldBe` 1

    it "polymorphic parameter product type" $ do
      productType <- generate (arbitraryAdt (Proxy :: Proxy (PolymorphicParameterProductType String)))
      let productTypeArbitraries = capArbitrary <$> adtCAPs productType

      length productTypeArbitraries `shouldBe` 1
