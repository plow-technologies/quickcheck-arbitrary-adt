{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TypeOperators     #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE DefaultSignatures   #-}

{-# LANGUAGE DeriveGeneric #-}

module Test.QuickCheck.Arbitrary.ADT where

import           GHC.Generics

import           Safe (headMay)

import           Test.QuickCheck.Gen
import           Test.QuickCheck.Arbitrary

import           Data.Typeable

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

-- | this data type makes the code easier to read than just using a tuple
data ConstructorArbitraryPair a = ConstructorArbitraryPair {
  _capConstructor :: String
, _capArbitrary   :: a
} deriving (Eq,Generic,Read,Show,Typeable)

-- fmap f is applied to `_capArbitrary`, this makes coding Generic easier
instance Functor ConstructorArbitraryPair where
  fmap f (ConstructorArbitraryPair c a) = ConstructorArbitraryPair c (f a)

instance (Arbitrary a) => Arbitrary (ConstructorArbitraryPair a) where
  arbitrary = ConstructorArbitraryPair <$> arbitrary <*> arbitrary

-- | ADTArbitrarySingleton is used for creating an arbitrary value of an
-- Algebraic Data Type for a random constructor
data ADTArbitrarySingleton a = ADTArbitrarySingleton {
  _adtasTypeName :: String
, _adtasCAP      :: ConstructorArbitraryPair a
} deriving (Eq,Generic,Read,Show,Typeable)

-- | fmap on tuple applies the function to the second value
instance Functor ADTArbitrarySingleton where
  fmap f (ADTArbitrarySingleton t c) = ADTArbitrarySingleton t (f <$> c)

instance (Arbitrary a) => Arbitrary (ADTArbitrarySingleton a) where
  arbitrary = ADTArbitrarySingleton <$> arbitrary <*> arbitrary


-- | ADTArbitrary is used for creating a list of arbitrary values for each
-- constructor in the type `a`.
data ADTArbitrary a = ADTArbitrary {
  _adtTypeName :: String
, _adtCAPs     :: [ConstructorArbitraryPair a]
} deriving (Eq,Generic,Read,Show,Typeable)

-- | fmap f <$> cs will apply f to the second value of every tuple in cs
instance Functor ADTArbitrary where
  fmap f (ADTArbitrary t cs) = ADTArbitrary t (fmap f <$> cs)

instance (Arbitrary a) => Arbitrary (ADTArbitrary a) where
  arbitrary = ADTArbitrary <$> arbitrary <*> arbitrary

genericToADTArbitrarySingleton :: forall a.
                                  ( Arbitrary a
                                  , Generic a
                                  , GToADTArbitrarySingleton (Rep a)
                                  , GToADTArbitrarySingleton (Rep (ADTArbitrarySingleton a)))
                                 => Proxy a
                                 -> Gen (ADTArbitrarySingleton a)
genericToADTArbitrarySingleton _ = fmap to <$> gToADTArbitrarySingleton (Proxy :: Proxy (Rep a))

class GToADTArbitrarySingleton rep where
  gToADTArbitrarySingleton :: Proxy rep -> Gen (ADTArbitrarySingleton (rep a))

instance GToADTArbitrarySingleton U1 where
  gToADTArbitrarySingleton _ = pure $ ADTArbitrarySingleton "" $ ConstructorArbitraryPair "" U1

instance (GToADTArbitrarySingleton l, GToADTArbitrarySingleton r) => GToADTArbitrarySingleton (l :+: r) where
  gToADTArbitrarySingleton _ = do
    b <- arbitrary
    if b then fmap L1 <$> gToADTArbitrarySingleton (Proxy :: Proxy l)
         else fmap R1 <$> gToADTArbitrarySingleton (Proxy :: Proxy r)

instance (GToADTArbitrarySingleton l, GToADTArbitrarySingleton r) => GToADTArbitrarySingleton (l :*: r) where
  gToADTArbitrarySingleton _ = do
    x <- getArb <$> gToADTArbitrarySingleton (Proxy :: Proxy l)
    y <- getArb <$> gToADTArbitrarySingleton (Proxy :: Proxy r)
    return $ ADTArbitrarySingleton  "" $ ConstructorArbitraryPair "" (x :*: y)
    where
      getArb = _capArbitrary . _adtasCAP

instance Arbitrary a => GToADTArbitrarySingleton (K1 i a) where
  gToADTArbitrarySingleton _ = ADTArbitrarySingleton
                           <$> pure ""
                           <*>  ( ConstructorArbitraryPair
                              <$> pure ""
                              <*> K1 <$> arbitrary
                                )

instance (Constructor c, GToADTArbitrarySingleton rep) => GToADTArbitrarySingleton (M1 C c rep) where
  gToADTArbitrarySingleton _ = ADTArbitrarySingleton
                           <$> pure ""
                           <*> ( ConstructorArbitraryPair con <$> ac)
    where
      kRep = gToADTArbitrarySingleton (Proxy :: Proxy rep)
      ac   = M1 . _capArbitrary . _adtasCAP <$> kRep
      con = conName (undefined :: M1 C c rep ())

instance (Datatype t, Typeable t, GToADTArbitrarySingleton rep) => GToADTArbitrarySingleton (M1 D t rep) where
  gToADTArbitrarySingleton _ =  ADTArbitrarySingleton
                             <$> pure t
                             <*>  ( ConstructorArbitraryPair
                                 <$> (_capConstructor . _adtasCAP <$> kRep)
                                 <*> ac)
    where
      kRep = gToADTArbitrarySingleton (Proxy :: Proxy rep)
      ac   = M1 . _capArbitrary . _adtasCAP <$> kRep
      t    = datatypeName (undefined :: M1 D t rep ())

instance GToADTArbitrarySingleton rep => GToADTArbitrarySingleton (M1 S t rep) where
  gToADTArbitrarySingleton _ = ADTArbitrarySingleton
                            <$> pure ""
                            <*>  ( ConstructorArbitraryPair
                                <$> pure ""
                                <*> ac)
    where
      kRep = gToADTArbitrarySingleton (Proxy :: Proxy rep)
      ac   = M1 . _capArbitrary . _adtasCAP <$> kRep

---


data ArbitraryConstructorData a = ArbitraryConstructorData {
  typeName             :: String
, arbitraryConstructor :: (String, a)
} deriving (Generic,Eq,Show)--,Functor) -- functor applies to a

instance Functor ArbitraryConstructorData where
  fmap f (ArbitraryConstructorData typeName (aS,a)) = ArbitraryConstructorData typeName (aS, f a)

genericToArbitraryConstructorT :: forall a. (Arbitrary a, Generic a, GToArbitraryConstructorT (Rep a), GToArbitraryConstructorT (Rep (ArbitraryConstructorData a))) => Proxy a -> Gen (ArbitraryConstructorData a)
genericToArbitraryConstructorT _ = fmap to <$> gToArbitraryConstructorT (Proxy :: Proxy (Rep a))

class GToArbitraryConstructorT rep where
  gToArbitraryConstructorT :: Proxy rep -> Gen (ArbitraryConstructorData (rep a))

instance GToArbitraryConstructorT U1 where
  gToArbitraryConstructorT _ = pure $ ArbitraryConstructorData "" ("",U1)

instance (GToArbitraryConstructorT l, GToArbitraryConstructorT r) => GToArbitraryConstructorT (l :+: r) where
  gToArbitraryConstructorT _ = do
    b <- arbitrary
    if b then fmap L1 <$> gToArbitraryConstructorT (Proxy :: Proxy l)
         else fmap R1 <$> gToArbitraryConstructorT (Proxy :: Proxy r)

instance (GToArbitraryConstructorT l, GToArbitraryConstructorT r) => GToArbitraryConstructorT (l :*: r) where
  gToArbitraryConstructorT _ = do
    x <- arbitraryConstructor <$> gToArbitraryConstructorT (Proxy :: Proxy l)
    y <- arbitraryConstructor <$> gToArbitraryConstructorT (Proxy :: Proxy r)
    return $ ArbitraryConstructorData  "" ("", snd x :*: snd y)

instance Arbitrary a => GToArbitraryConstructorT (K1 i a) where
  gToArbitraryConstructorT _ = ArbitraryConstructorData <$> pure "" <*> ((,) "" . K1 <$> arbitrary)

instance (Constructor c, GToArbitraryConstructorT rep) => GToArbitraryConstructorT (M1 C c rep) where
  gToArbitraryConstructorT _ = ArbitraryConstructorData <$> pure "" <*> ((,) con <$> ac)
    where
      kRep = gToArbitraryConstructorT (Proxy :: Proxy rep)
      ac   = M1 . snd . arbitraryConstructor <$> kRep
      con = conName (undefined :: M1 C c rep ())

instance (Datatype t, Typeable t, GToArbitraryConstructorT rep) => GToArbitraryConstructorT (M1 D t rep) where
  gToArbitraryConstructorT _ = ArbitraryConstructorData <$> pure t <*> ((,) <$> (fst . arbitraryConstructor <$> z) <*> (M1 . snd . arbitraryConstructor <$> z))
    where
      z = gToArbitraryConstructorT (Proxy :: Proxy rep)
      t = datatypeName (undefined :: M1 D t rep ())

instance GToArbitraryConstructorT rep => GToArbitraryConstructorT (M1 S t rep) where
  gToArbitraryConstructorT _ = ArbitraryConstructorData <$> pure "" <*> ((,) <$> (fst . arbitraryConstructor <$> z) <*> (M1 . snd . arbitraryConstructor <$> z))
    where
      z = gToArbitraryConstructorT (Proxy :: Proxy rep)


class TypeName a where
  typename :: Proxy a -> String

  default typename :: (Generic a, GTypeName (Rep a)) => Proxy a -> String
  typename _proxy = gtypename (from (undefined :: a))

-- | Generic equivalent to `TypeName`.
class GTypeName f where
  gtypename :: f a -> String

--instance (Datatype c) => GTypeName (M1 i c f) where
--  gtypename m = datatypeName (undefined :: M1 i c f ())

instance (Datatype c) => GTypeName (M1 C c f) where
  gtypename m = datatypeName (undefined :: M1 C c f ())

instance (Datatype c) => GTypeName (M1 D c f) where
  gtypename m = datatypeName (undefined :: M1 D c f ())

instance (Datatype c) => GTypeName (M1 S c f) where
  gtypename m = datatypeName (undefined :: M1 S c f ())
