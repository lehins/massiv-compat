{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Test.Massiv.SerialiseSpec (spec) where

import Codec.Serialise
import Common
import Massiv.Serialise ()
import Test.Massiv.Core
import Data.Massiv.Array
import Data.Word

roundtrip :: (Eq a, Show a, Serialise a) => a -> Property
roundtrip val = deserialise (serialise val) === val

roundtripArray ::
     forall r ix e.
     ( Load r ix e
     , Serialise (Array r ix e)
     , Eq (Array r ix e)
     , Show (Array r ix e)
     )
  => Array r ix e
  -> Property
roundtripArray arr =
  let arr' = deserialise (serialise arr)
  in (arr' === arr) .&&. (getComp arr' === getComp arr)

roundtripArraySpec ::
     forall r ix e.
     ( Eq (Array r ix e)
     , Show (Array r ix e)
     , Typeable ix
     , Typeable e
     , Arbitrary ix
     , Load r ix e
     , Arbitrary e
     , Serialise (Array r ix e)
     )
  => Spec
roundtripArraySpec =
  prop (showsType @(Array r ix e) "") $ roundtripArray @r @ix @e

spec :: Spec
spec = do
  describe "Serialise" $ do
    prop "Comp" $ roundtrip @Comp
    describe "Ix" $ do
      prop "Ix2" $ roundtrip @Ix2
      prop "Ix3" $ roundtrip @Ix3
      prop "Ix4" $ roundtrip @Ix4
      prop "Ix5" $ roundtrip @Ix5
    describe "Sz" $ do
      prop "Sz1" $ roundtrip @Sz1
      prop "Sz2" $ roundtrip @Sz2
      prop "Sz3" $ roundtrip @Sz3
      prop "Sz4" $ roundtrip @Sz4
      prop "Sz5" $ roundtrip @Sz5
    describe "Array" $ do
      describe "P" $ do
        roundtripArraySpec @P @Ix1 @Word8
        roundtripArraySpec @P @Ix2 @Word8
        roundtripArraySpec @P @Ix3 @Word8
        roundtripArraySpec @P @Ix4 @Word8
        roundtripArraySpec @P @Ix5 @Word8
        roundtripArraySpec @P @Ix1 @Word16
        roundtripArraySpec @P @Ix2 @Word16
        roundtripArraySpec @P @Ix3 @Word16
        roundtripArraySpec @P @Ix4 @Word16
        roundtripArraySpec @P @Ix5 @Word16
        roundtripArraySpec @P @Ix1 @Word32
        roundtripArraySpec @P @Ix2 @Word32
        roundtripArraySpec @P @Ix3 @Word32
        roundtripArraySpec @P @Ix4 @Word32
        roundtripArraySpec @P @Ix5 @Word32
        roundtripArraySpec @P @Ix1 @Word64
        roundtripArraySpec @P @Ix2 @Word64
        roundtripArraySpec @P @Ix3 @Word64
        roundtripArraySpec @P @Ix4 @Word64
        roundtripArraySpec @P @Ix5 @Word64
        roundtripArraySpec @P @Ix1 @Word
        roundtripArraySpec @P @Ix2 @Word
        roundtripArraySpec @P @Ix3 @Word
        roundtripArraySpec @P @Ix4 @Word
        roundtripArraySpec @P @Ix5 @Word
      describe "U" $ do
        roundtripArraySpec @U @Ix1 @Word
        roundtripArraySpec @U @Ix2 @Word
        roundtripArraySpec @U @Ix3 @Word
        roundtripArraySpec @U @Ix4 @Word
        roundtripArraySpec @U @Ix5 @Word
      describe "S" $ do
        roundtripArraySpec @S @Ix1 @Word
        roundtripArraySpec @S @Ix2 @Word
        roundtripArraySpec @S @Ix3 @Word
        roundtripArraySpec @S @Ix4 @Word
        roundtripArraySpec @S @Ix5 @Word
      describe "B" $ do
        roundtripArraySpec @B @Ix1 @Integer
        roundtripArraySpec @B @Ix2 @Integer
        roundtripArraySpec @B @Ix3 @Integer
        roundtripArraySpec @B @Ix4 @Integer
        roundtripArraySpec @B @Ix5 @Integer
      describe "BN" $ do
        roundtripArraySpec @BN @Ix1 @Integer
        roundtripArraySpec @BN @Ix2 @Integer
        roundtripArraySpec @BN @Ix3 @Integer
        roundtripArraySpec @BN @Ix4 @Integer
        roundtripArraySpec @BN @Ix5 @Integer
      describe "BL" $ do
        roundtripArraySpec @BL @Ix1 @Integer
        roundtripArraySpec @BL @Ix2 @Integer
        roundtripArraySpec @BL @Ix3 @Integer
        roundtripArraySpec @BL @Ix4 @Integer
        roundtripArraySpec @BL @Ix5 @Integer
