{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Common
  ( module X
  , roundtrip
  , roundtripArray
  , roundtripArraySpec
  , roundtripArraysSpec
  ) where

import Test.Hspec as X
import Test.Hspec.QuickCheck as X
import Test.QuickCheck as X
import Data.Persist as X
import Massiv.Persist as X
import Data.Typeable
import Data.Massiv.Array
import Test.Massiv.Core

roundtrip :: (Eq a, Show a, Persist a) => a -> Property
roundtrip val = either error id (decode (encode val)) === val

roundtripArray ::
     forall r ix e.
     ( Mutable r e
     , Index ix
     , Persist e
     , Persist (Array r ix e)
     , Eq (Array r ix e)
     , Show (Array r ix e)
     )
  => Array r ix e
  -> Property
roundtripArray arr = expectProp $ do
  let arr'' = either error id (runGet getArray (runPut (putArray arr)))
  getComp arr'' `shouldBe` getComp arr
  arr'' `shouldBe` arr
  let arr' = either error id (decode (encode arr))
  getComp arr' `shouldBe` getComp arr
  arr' `shouldBe` arr

roundtripArraySpec ::
     forall r ix e.
     ( Eq (Array r ix e)
     , Show (Array r ix e)
     , Typeable e
     , Arbitrary ix
     , Load r ix e
     , Mutable r e
     , Arbitrary e
     , Persist e
     , Persist (Array r ix e)
     )
  => Spec
roundtripArraySpec =
  prop (showsType @(Array r ix e) "") $ roundtripArray @r @ix @e


roundtripArraysSpec ::
     forall r e.
     ( Eq (Array r Ix1 e)
     , Show (Array r Ix1 e)
     , Persist (Array r Ix1 e)
     , Load r Ix1 e
     , Eq (Array r Ix2 e)
     , Show (Array r Ix2 e)
     , Persist (Array r Ix2 e)
     , Load r Ix2 e
     , Eq (Array r Ix3 e)
     , Show (Array r Ix3 e)
     , Persist (Array r Ix3 e)
     , Load r Ix3 e
     , Eq (Array r Ix4 e)
     , Show (Array r Ix4 e)
     , Load r Ix4 e
     , Persist (Array r Ix4 e)
     , Eq (Array r Ix5 e)
     , Show (Array r Ix5 e)
     , Persist (Array r Ix5 e)
     , Load r Ix5 e
     , Typeable e
     , Mutable r e
     , Arbitrary e
     , Persist e
     )
  => Spec
roundtripArraysSpec =
  describe (showsType @e "") $ do
    roundtripArraySpec @r @Ix1 @e
    roundtripArraySpec @r @Ix2 @e
    roundtripArraySpec @r @Ix3 @e
    roundtripArraySpec @r @Ix4 @e
    roundtripArraySpec @r @Ix5 @e
