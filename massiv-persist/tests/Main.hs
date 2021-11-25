module Main where

import System.IO (BufferMode(LineBuffering), hSetBuffering, hSetEncoding, stdout, utf8)
import Test.Hspec
import qualified Test.Massiv.Persist.BoxedSpec as Boxed (spec)
import qualified Test.Massiv.Persist.CoreSpec as Core (spec)
import qualified Test.Massiv.Persist.PrimitiveSpec as Primitive (spec)
import qualified Test.Massiv.Persist.StorableSpec as Storable (spec)
import qualified Test.Massiv.Persist.UnboxedSpec as Unboxed (spec)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetEncoding stdout utf8
  hspec $ do
    describe "Core" $ do
      Core.spec
    describe "Boxed" $ do
      Boxed.spec
    describe "Unboxed" $ do
      Primitive.spec
      Storable.spec
      Unboxed.spec
