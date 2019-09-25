module Main where

import           Test.Hspec (hspec)
import qualified Test.QuickCheck.Arbitrary.ADT.LegacySpec as Legacy
import qualified Test.QuickCheck.Arbitrary.ADTSpec as ADT

main :: IO ()
main = do
  hspec Legacy.spec
  hspec ADT.spec
