{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}

module Test.QuickCheck.Arbitrary.ADTSpec (main, spec) where

-- base
import Data.ByteString (ByteString)
import Data.Maybe (isJust)
import Data.Proxy
import GHC.Generics
-- hspec
import Test.Hspec
-- lens
import Control.Lens
-- QuickCheck
import Test.QuickCheck
-- quickcheck-adt-arbitrary
import Test.QuickCheck.Arbitrary.ADT

newtype MyString = MyString ByteString
  deriving (Eq,Generic,Show)

-- instance ToADTArbitrary MyString
instance Arbitrary MyString where
  arbitrary = pure (MyString "Hello")

instance ToADTArbitrary MyString where
  toADTArbitrarySingleton Proxy =
    ADTArbitrarySingleton "Test.QuickCheck.Arbitrary.ADTSpec" "MyString"
      <$> oneof
        [ ConstructorArbitraryPair "MyString" <$> arbitrary
        ]

  toADTArbitrary Proxy =
    ADTArbitrary "Test.QuickCheck.Arbitrary.ADTSpec" "MyString"
      <$> sequence
        [ ConstructorArbitraryPair "MyString" <$> arbitrary
        ]

-- | A tagless type has constructor which has no parameters. It has U1 in its
-- `GHC.Generics` representation.
data TaglessType = TaglessType
  deriving (Eq,Generic,Show)

instance ToADTArbitrary TaglessType
instance Arbitrary TaglessType where
  arbitrary = genericArbitrary

$(makePrisms ''TaglessType)

-- | A sum type (disjoin union, tagged union) has multiple type constructors
-- that all result in values of different shapes, but the same type. It has
-- `:+:` and `M1 C c rep` in its `GHC.Generics` representation.
data SumType
  = SumType1 Int
  | SumType2 String Int
  | SumType3 String [Int] Double
  | SumType4 String [String] [Int] Double
  deriving (Eq,Generic,Show)

instance ToADTArbitrary SumType
instance Arbitrary SumType where
  arbitrary = genericArbitrary

$(makePrisms ''SumType)

-- | This type is an example of a sum type that has constructors building sum
-- types.
data SumOfSums
  = SSBareSumType TaglessType
  | SSSumType SumType
  deriving (Eq,Generic,Show)

instance ToADTArbitrary SumOfSums
instance Arbitrary SumOfSums where
  arbitrary = genericArbitrary

$(makePrisms ''SumOfSums)

-- | A product type has one constructor with one records. It has :*: and `M1 S s rep`
-- in its `GHC.Generics` representation.

data ProductType =
  ProductType
    { name :: String
    , age  :: Int
    } deriving (Eq,Generic,Show)

instance ToADTArbitrary ProductType

data PolymorphicParameterProductType a =
  PolymorphicParameterProductType
    { firstItem  :: Int
    , secondItem :: a
    } deriving (Eq,Show,Generic)

instance ToADTArbitrary (PolymorphicParameterProductType String)

data NonGenericSumType
  = NGSumType1 Int
  | NGSumType2 String Int
  | NGSumType3 String [Int] Double
  | NGSumType4 String [String] [Int] Double
  deriving (Eq, Show)

instance ToADTArbitrary NonGenericSumType where
  toADTArbitrarySingleton Proxy =
    ADTArbitrarySingleton "Test.QuickCheck.Arbitrary.ADTSpec" "NonGenericSum"
      <$> oneof
        [ ConstructorArbitraryPair "NGSumType1" <$> (NGSumType1 <$> arbitrary)
        , ConstructorArbitraryPair "NGSumType2" <$> (NGSumType2 <$> arbitrary <*> arbitrary)
        , ConstructorArbitraryPair "NGSumType3" <$> (NGSumType3 <$> arbitrary <*> arbitrary <*> arbitrary)
        , ConstructorArbitraryPair "NGSumType4" <$> (NGSumType4 <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary)
        ]

  toADTArbitrary Proxy =
    ADTArbitrary "Test.QuickCheck.Arbitrary.ADTSpec" "NonGenericSum"
      <$> sequence
        [ ConstructorArbitraryPair "NGSumType1" <$> (NGSumType1 <$> arbitrary)
        , ConstructorArbitraryPair "NGSumType2" <$> (NGSumType2 <$> arbitrary <*> arbitrary)
        , ConstructorArbitraryPair "NGSumType3" <$> (NGSumType3 <$> arbitrary <*> arbitrary <*> arbitrary)
        , ConstructorArbitraryPair "NGSumType4" <$> (NGSumType4 <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary)
        ]

$(makePrisms ''NonGenericSumType)

data NonGenericProductType =
  NonGenericProductType 
    { ngName :: String
    , ngAge  :: Int
    } deriving (Eq,Generic,Show)

instance ToADTArbitrary NonGenericProductType where
  toADTArbitrarySingleton Proxy =
    ADTArbitrarySingleton "Test.QuickCheck.Arbitrary.ADTSpec" "NonGenericSum"
      <$> (ConstructorArbitraryPair "NonGenericProductType" <$> (NonGenericProductType <$> arbitrary <*> arbitrary))

  toADTArbitrary Proxy =
    ADTArbitrary "Test.QuickCheck.Arbitrary.ADTSpec" "NonGenericProductType"
      <$> sequence
        [ ConstructorArbitraryPair "NonGenericProductType" <$> (NonGenericProductType <$> arbitrary <*> arbitrary)
        ]

spec :: Spec
spec = do
  describe "QuickCheck Arbitrary ADT: Generic ToADTArbitrary type class instance" $ do
    it "toADTArbitrary of a tagless type should create an instance of the bare constructor" $ do
      _                    <- generate (toADTArbitrarySingleton (Proxy :: Proxy TaglessType))
      taglessType          <- generate (toADTArbitrary (Proxy :: Proxy TaglessType))
      let taglessTypeArbitraries = capArbitrary <$> adtCAPs taglessType
      or (isJust . preview _TaglessType <$> taglessTypeArbitraries) `shouldBe` True

    it "toADTArbitrary of a sum type creates an instance with each constructor" $ do
      sumTypeCAP <- capArbitrary . adtasCAP <$> generate (toADTArbitrarySingleton (Proxy :: Proxy SumType))
      sumTypes   <- generate (toADTArbitrary (Proxy :: Proxy SumType))
      let sumTypeArbitraries = capArbitrary <$> adtCAPs sumTypes

      or
        [ isJust . preview _SumType1 $ sumTypeCAP
        , isJust . preview _SumType2 $ sumTypeCAP
        , isJust . preview _SumType3 $ sumTypeCAP
        , isJust . preview _SumType4 $ sumTypeCAP
        ] `shouldBe` True
        
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
      let sumOfSumsArbitraries = capArbitrary <$> adtCAPs sumOfSums
      and
        [ or $ isJust . preview _SSBareSumType <$> sumOfSumsArbitraries
        , or $ isJust . preview _SSSumType <$> sumOfSumsArbitraries
        , length sumOfSumsArbitraries == 2
        ] `shouldBe` True

    it "toADTArbitrary of a product type creates a single instance" $ do
      _           <- generate (toADTArbitrarySingleton (Proxy :: Proxy ProductType))
      productType <- generate (toADTArbitrary (Proxy :: Proxy ProductType))
      let productTypeArbitraries = capArbitrary <$> adtCAPs productType

      length productTypeArbitraries `shouldBe` 1

    it "toADTArbitrary of a product type that has a polymorphic parameter should work as well" $ do
      _           <- generate (toADTArbitrarySingleton (Proxy :: Proxy (PolymorphicParameterProductType String)))
      productType <- generate (toADTArbitrary (Proxy :: Proxy (PolymorphicParameterProductType String)))
      let productTypeArbitraries = capArbitrary <$> adtCAPs productType

      length productTypeArbitraries `shouldBe` 1

  describe "QuickCheck Arbitrary ADT: ToADTArbitrary type class instance without Generics" $ do
    it "toADTArbitrary of a sum type creates an instance with each constructor" $ do
      sumTypeCAP <- capArbitrary . adtasCAP <$> generate (toADTArbitrarySingleton (Proxy :: Proxy NonGenericSumType))
      sumTypes   <- generate (toADTArbitrary (Proxy :: Proxy NonGenericSumType))
      let sumTypeArbitraries = capArbitrary <$> adtCAPs sumTypes

      or
        [ isJust . preview _NGSumType1 $ sumTypeCAP
        , isJust . preview _NGSumType2 $ sumTypeCAP
        , isJust . preview _NGSumType3 $ sumTypeCAP
        , isJust . preview _NGSumType4 $ sumTypeCAP
        ] `shouldBe` True

      and
        [ or $ isJust . preview _NGSumType1 <$> sumTypeArbitraries
        , or $ isJust . preview _NGSumType2 <$> sumTypeArbitraries
        , or $ isJust . preview _NGSumType3 <$> sumTypeArbitraries
        , or $ isJust . preview _NGSumType4 <$> sumTypeArbitraries
        , length sumTypeArbitraries == 4
        ] `shouldBe` True

    it "toADTArbitrary of a product type creates a single instance" $ do
      _           <- generate (toADTArbitrarySingleton (Proxy :: Proxy NonGenericProductType))
      productType <- generate (toADTArbitrary (Proxy :: Proxy NonGenericProductType))
      let productTypeArbitraries = capArbitrary <$> adtCAPs productType

      length productTypeArbitraries `shouldBe` 1

main :: IO ()
main = hspec spec
