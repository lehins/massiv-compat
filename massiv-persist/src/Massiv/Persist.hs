{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Massiv.Persist
-- Copyright   : (c) Alexey Kuleshevich 2021
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
-- This package provides instances for `Persist` class for all mutable `Array`
-- representations in [@massiv@](https://hackage.haskell.org/package/massiv) package. These
-- instances are provided as orphans from a separate package in order to avoid direct
-- dependency on [@persist@](https://hackage.haskell.org/package/persist) package in
-- @massiv@.
--
-- Array elements are serialized in little endian order, which is consistent with the rest
-- of the instances in @persist@ package.
--
-- Below is a simple example how to use it. Note a blank module import: @import
-- Massiv.Persist ()@, which is the only thing needed from this module in order to use
-- provided orphan instances.
--
-- >>> import Massiv.Persist ()
-- >>> import Data.Massiv.Array as A
-- >>> import Data.Int (Int8)
-- >>> let arr = A.fromList A.Seq [72,97,115,107,101,108,108] :: A.Vector A.P Int8
-- >>> encode arr
-- "\NUL\a\NUL\NUL\NUL\NUL\NUL\NUL\NULHaskell"
-- >>> Right arr' = decode (encode arr) :: Either String (A.Vector A.P Int8)
-- >>> arr'
-- Array P Seq (Sz1 7)
--   [ 72, 97, 115, 107, 101, 108, 108 ]
--
module Massiv.Persist
  ( -- * Helper functions used to define Persist instances
    putIx
  , getIx
  , mkSzFail
  , putArray
  , getArray
  , putPrimArray
  , getPrimArray
  , putStorableArray
  , getStorableArray
  ) where

import Control.DeepSeq (NFData)
import Control.Monad
import qualified Control.Monad.Fail as Fail
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Short.Internal as SBS
import Data.Foldable as F
import Data.Massiv.Array as A
import Data.Massiv.Array.Unsafe
import Data.Persist
import Data.Persist.Internal
import qualified Data.Primitive as Primitive
import qualified Data.Primitive.ByteArray as BA
import Data.Proxy
import Data.Word
import Foreign.Ptr
import Foreign.ForeignPtr
import qualified Foreign.Storable as Storable

#include "MachDeps.h"

instance Persist Comp where
  put comp =
    case comp of
      Seq      -> put (0 :: Word8)
      ParOn xs -> put (1 :: Word8) >> put xs
      ParN n   -> put (2 :: Word8) >> put n
  get = do
    ty :: Word8 <- get
    case ty of
      0 -> pure Seq
      1 -> ParOn <$> get
      2 -> ParN <$> get
      n -> Fail.fail $ "Unexpected Comp tag: " <> show n



-- | Encode index
--
-- @since 0.1.0
putIx ::
     forall ix. Index ix
  => ix
  -> Put ()
putIx = foldlIndex (\ !acc i -> put i >> acc) (pure ())

-- | Decode index
--
-- @since 0.1.0
getIx ::
     forall ix. Index ix
  => Get ix
getIx = do
  let getDim ix dim = do
        i <- get
        either (Fail.fail . show) pure $! setDimM ix dim i
  F.foldlM getDim zeroIndex [1 .. dimensions (Proxy :: Proxy ix)]

instance Persist Ix2 where
  put = putIx
  get = getIx

instance Index (IxN n) => Persist (IxN n) where
  put = putIx
  get = getIx



-- | Construct size from index verifying its correctness.
--
-- @since 0.1.0
mkSzFail ::
     forall ix m. (Index ix, Fail.MonadFail m)
  => ix
  -> m (Sz ix)
mkSzFail ix = do
  let guardNegativeOverflow i !acc = do
        when (i < 0) $ Fail.fail $ "Negative size encountered: " <> show i
        let acc' = i * acc
        when (acc' /= 0 && acc' < acc) $ Fail.fail $ "Overflow detected, size is too big: " <> show i
        pure acc'
  Sz ix <$ foldlIndex (\acc i -> acc >>= guardNegativeOverflow i) (pure 1) ix

instance Index ix => Persist (Sz ix) where
  put = putIx . unSz
  get = mkSzFail =<< getIx



-- | Serialize array computation startegy and size
putArrayHeader ::
     forall r ix e. Manifest r ix e
  => Array r ix e
  -> Put ()
putArrayHeader arr = do
  put (getComp arr)
  put (size arr)

instance (Index ix, Persist e) => Persist (Array B ix e) where
  put = putArray
  get = getArray

instance (Index ix, NFData e, Persist e) => Persist (Array N ix e) where
  put = putArray
  get = getArray

instance (Index ix, Unbox e, Persist e) => Persist (Array U ix e) where
  put = putArray
  get = getArray

-- | Serialize array as `LittleEndian`
--
-- @since 0.1.0
putArray :: (Manifest r ix e, Persist e) => Array r ix e -> Put ()
putArray arr = do
  putArrayHeader arr
  A.mapM_ put arr

-- | Deserialize array from binary form in `LittleEndian`
--
-- @since 0.1.0
getArray :: (Mutable r ix e, Persist e) => Get (Array r ix e)
getArray = do
  comp <- get
  sz <- get
  setComp comp <$> A.makeArrayA sz (const get)

-- | Serialize primitive array in `LittleEndian` order
--
-- @since 0.1.0
putPrimArray :: forall ix e . (Index ix, Prim e, Persist e) => Array P ix e -> Put ()
putPrimArray arr = do
#ifdef WORDS_BIGENDIAN
  putArray
#else
  putArrayHeader arr
  let eltBytes = Primitive.sizeOf (undefined :: e)
      n = totalElem (size arr) * eltBytes
      _ = put (undefined :: e) -- force `Persist` constraint
      s =
        case unwrapByteArray arr of
          BA.ByteArray ba -> SBS.SBS ba
  grow n
  Put $ \_ p -> do
    SBS.copyToPtr s (unwrapByteArrayOffset arr * eltBytes) p n
    pure $! p `plusPtr` n :!: ()
#endif

-- | Deserialize primitive array in `LittleEndian` order
--
-- @since 0.1.0
getPrimArray :: forall ix e . (Index ix, Prim e, Persist e) => Get (Array P ix e)
getPrimArray = do
#ifdef WORDS_BIGENDIAN
  getArray
#else
  comp <- get
  sz <- get
  let n = totalElem sz * Primitive.sizeOf (undefined :: e)
      _ = put (undefined :: e) -- force `Persist` constraint
  SBS.SBS sbs <- SBS.toShort <$!> getBytes n
  either (Fail.fail . show) pure $
    resizeM sz (fromByteArray comp (BA.ByteArray sbs))
#endif

instance (Index ix, Prim e, Persist e) => Persist (Array P ix e) where
  put = putPrimArray
  get = getPrimArray


-- | Serialize storable array in `LittleEndian` order
--
-- @since 0.1.0
putStorableArray :: forall ix e . (Index ix, Storable e, Persist e) => Array S ix e -> Put ()
putStorableArray arr = do
#ifdef WORDS_BIGENDIAN
  puArray
#else
  let _ = put (undefined :: e) -- force `Persist` constraint
  putArrayHeader arr
  putByteString $!
    case unsafeArrayToForeignPtr arr of
      (fp, len) ->
        BS.PS (castForeignPtr fp) 0 (len * Storable.sizeOf (undefined :: e))
#endif

-- | Deserialize storable array in `LittleEndian` order
--
-- @since 0.1.0
getStorableArray :: forall ix e . (Index ix, Storable e, Persist e) => Get (Array S ix e)
getStorableArray = do
#ifdef WORDS_BIGENDIAN
  getArray
#else
  comp <- get
  sz <- get
  let eltCount = totalElem sz
      eltBytes = Storable.sizeOf (undefined :: e)
      _ = put (undefined :: e) -- force `Persist` constraint
  BS.PS fp off _ <- getByteString (eltCount * eltBytes)
  either (Fail.fail . show) pure $
    resizeM sz $
    unsafeArrayFromForeignPtr0 comp (fp `plusForeignPtr` off) (Sz eltCount)
#endif

instance (Index ix, Storable e, Persist e) => Persist (Array S ix e) where
  put = putStorableArray
  get = getStorableArray
