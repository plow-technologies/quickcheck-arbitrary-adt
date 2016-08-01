{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TypeOperators     #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE DefaultSignatures   #-}

module Test.QuickCheck.Arbitrary.ADT where

import           GHC.Generics

import           Safe (headMay)

import           Test.QuickCheck.Gen
import           Test.QuickCheck.Arbitrary

-- | ToArbitraryConstructor type class provides to functions.
-- `toArbitraryConstructor` creates an arbitrary instance of one of the
-- constructors of a given type.
-- `toArbitraryConstructorList` creates an arbitrary instance of each of the
-- constructors of a given type.
-- If you use the default implementation with generic derivation,
-- `toArbitraryConstructorList` will automatically provide an arbitrary for
-- each constructor.
-- If you define it yourself, it is your responsibility to make sure that
-- an arbitrary instance of each type constructor is provided. The type signature
-- does not guarantee that there will an instance of each type constructor.

class ToArbitraryConstructor a where
  toArbitraryConstructor :: Gen (String,a)
  default toArbitraryConstructor :: (Generic a, GToArbitraryConstructor (Rep a)) => Gen (String, a)
  toArbitraryConstructor = fmap to <$> gToArbitraryConstructor

  toArbitraryConstructorList :: Gen [(String,a)]
  default toArbitraryConstructorList :: (Generic a, GToArbitraryConstructorList (Rep a)) => Gen [(String, a)]
  toArbitraryConstructorList = (fmap . fmap) to <$> gToArbitraryConstructorList

-- | GArbitrary is a typeclass for generalizing the creation of single arbitrary
-- product and sum types. Assume a simple sum type
-- data SumType = SumTypeFirst  String
--              | SumTypeSecond String
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

genericArbitrary :: (Generic a, GArbitrary (Rep a)) => Gen a
genericArbitrary = to <$> gArbitrary



-- | GToArbitraryConstructor creates an arbitrary and returns the name of the
-- constructor that was used to create it

class GToArbitraryConstructor rep where
  gToArbitraryConstructor :: Gen (String, rep a)

instance GToArbitraryConstructor U1 where
  gToArbitraryConstructor = pure ("",U1)

instance (GToArbitraryConstructor l, GToArbitraryConstructor r) => GToArbitraryConstructor (l :+: r) where
  gToArbitraryConstructor = do
    b <- arbitrary
    if b then fmap L1 <$> gToArbitraryConstructor
         else fmap R1 <$> gToArbitraryConstructor

instance (GToArbitraryConstructor l, GToArbitraryConstructor r) => GToArbitraryConstructor (l :*: r) where
  gToArbitraryConstructor = do
    (_,x) <- gToArbitraryConstructor
    (_,y) <- gToArbitraryConstructor
    return ("", x :*: y)

instance Arbitrary a => GToArbitraryConstructor (K1 i a) where
  gToArbitraryConstructor = (,) "" . K1 <$> arbitrary

instance (Constructor c, GToArbitraryConstructor rep) => GToArbitraryConstructor (M1 C c rep) where
  gToArbitraryConstructor = fmap M1 <$> ((,) <$> pure con <*> (snd <$> gToArbitraryConstructor))
    where
      con = conName (undefined :: M1 C c rep ())

instance GToArbitraryConstructor rep => GToArbitraryConstructor (M1 D t rep) where
  gToArbitraryConstructor = fmap M1 <$> gToArbitraryConstructor

instance GToArbitraryConstructor rep => GToArbitraryConstructor (M1 S t rep) where
  gToArbitraryConstructor = fmap M1 <$> gToArbitraryConstructor


-- | GToArbitraryConstructorList is a typeclass for generalizing the creation
-- of a list of arbitrary instances of each constructor of a type.  It also
-- includes the name of the constructor as a String for reference and file
-- creation.

class GToArbitraryConstructorList rep where
  gToArbitraryConstructorList :: Gen [(String, rep a)]

instance GToArbitraryConstructorList U1 where
  gToArbitraryConstructorList = pure [("",U1)]

instance (GToArbitraryConstructorList l, GToArbitraryConstructorList r) => GToArbitraryConstructorList (l :+: r) where
  gToArbitraryConstructorList = (++) <$> ((fmap . fmap) L1 <$> gToArbitraryConstructorList) <*> ((fmap . fmap) R1 <$> gToArbitraryConstructorList)

instance (GToArbitraryConstructor l, GToArbitraryConstructorList l, GToArbitraryConstructor r, GToArbitraryConstructorList r) => GToArbitraryConstructorList (l :*: r) where
  gToArbitraryConstructorList = do
    xs <- gToArbitraryConstructor
    ys <- gToArbitraryConstructor
    return [("", snd xs :*: snd ys)]

{-
instance (Constructor c, GToArbitraryConstructorList rep) => GToArbitraryConstructorList (M1 C c rep) where
  gToArbitraryConstructorList = (fmap . fmap) M1 . (:[]) <$> ((,) <$> pure con <*> (snd . head <$> gToArbitraryConstructorList))
    where
      con = conName (undefined :: M1 C c rep ())
-}

instance (Constructor c, GToArbitraryConstructorList rep) => GToArbitraryConstructorList (M1 C c rep) where
  gToArbitraryConstructorList = do
    mConRep <- fmap snd . headMay <$> gToArbitraryConstructorList
    case mConRep of
      Nothing     -> fail "GToArbitraryConstructorList constructor representation returned an empty list. This should not happen."
      Just conRep -> return $  fmap M1 <$> [(conString, conRep)]
    where
      conString = conName (undefined :: M1 C c rep ())


instance GToArbitraryConstructorList rep => GToArbitraryConstructorList (M1 D t rep) where
  gToArbitraryConstructorList = (fmap . fmap) M1 <$> gToArbitraryConstructorList

instance GToArbitraryConstructorList rep => GToArbitraryConstructorList (M1 S t rep) where
  gToArbitraryConstructorList = (fmap . fmap) M1 <$> gToArbitraryConstructorList

-- | at the Rep level there is no access to the constructor name
-- pass the arbitrary for the constructor with an empty string
-- the empty string will be replaced at the declaration for `M1 C c rep`
instance Arbitrary a => GToArbitraryConstructorList (K1 i a) where
  gToArbitraryConstructorList = (:[]) . (,) "" . K1 <$> arbitrary


--class AribitrariesWithConstructors a where
--  arbitrariesWithConstructors :: Gen [(String,a)]
--  default arbitrariesWithConstructors :: (Generic a, GArbitraryWithConList (Rep a)) => Gen [(String, a)]
--  arbitrariesWithConstructors = (fmap . fmap) to <$> garbitraryWithConList
-- let y = (:[]) <$> ((,) <$> pure "asdf" <*> x)

{-
instance (Selector s, Typeable t) => Selectors (M1 S s (K1 R t)) where
  selectors _ =
    [ ( selName (undefined :: M1 S s (K1 R t) ()) , typeOf (undefined :: t) ) ]

-}

--instance GArbitraryList rep => GArbitraryList (M1 i t rep) where
--  garbitraryList = fmap M1 <$> garbitraryList

--genericArbitraryList :: (Generic a, GArbitrary (Rep a), GArbitraryList (Rep a)) => Gen [a]
--genericArbitraryList = fmap to <$> garbitraryList


{- Inspect tree
data Sample = Sample1 Int | Sample2 Int String deriving (Eq,Show,Generic)
λ> :kind! Rep Sample ()
Rep Sample () :: *
= D1
    D1Sample
    (C1 C1_0Sample (S1 NoSelector (Rec0 Int))
     :+: C1
           C1_1Sample
           (S1 NoSelector (Rec0 Int) :*: S1 NoSelector (Rec0 String)))
    ()


:set -XDeriveGeneric
import GHC.Generics
data SumType = SumType1  Int
             | SumType2 String Int
             -- | SumType3  String [Int] Double
             -- | SumType4 String [String] [Int] Double
  deriving (Eq,Generic,Show)
:kind! Rep SumType ()
Rep SumType () :: *
= D1
    D1SumType
    (C1 C1_0SumType (S1 NoSelector (Rec0 Int))
     :+: C1
           C1_1SumType
           (S1 NoSelector (Rec0 String) :*: S1 NoSelector (Rec0 Int)))
    ()
-}
