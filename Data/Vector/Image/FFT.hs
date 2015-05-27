{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE IncoherentInstances  #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Vector.Image.FFT (
  fft1d
, fft2d
, fft3d
, Direction(..)
) where

import Prelude hiding(map)
import qualified Prelude as P
import qualified Data.List as L
import Data.Vector.Storable hiding(forM_,map,replicate,fromList,toList,(!?),(!))
import Data.Maybe
import Numeric.LinearAlgebra.Data hiding (fromList,toList,(!))
import Numeric.LinearAlgebra.HMatrix hiding (fromList,toList,(!))
import qualified Data.Vector.Storable as V
import Foreign
import GHC.Real
import Foreign.C.String
import Foreign.C.Types
import qualified Control.Monad as C
import Data.Vector.Image.Color.RGB
import System.IO.Unsafe(unsafePerformIO)

foreign import ccall unsafe "fft1d" c_fft1d :: Ptr (Complex Double) -> CInt -> CInt -> IO (Ptr (Complex Double))
foreign import ccall unsafe "fft2d" c_fft2d :: Ptr (Complex Double) -> CInt -> CInt -> CInt -> IO (Ptr (Complex Double))
foreign import ccall unsafe "fft3d" c_fft3d :: Ptr (Complex Double) -> CInt -> CInt -> CInt -> CInt -> IO (Ptr (Complex Double))
foreign import ccall unsafe "&fftw_free" c_p_freeComplexDouble :: FunPtr(Ptr (Complex Double) -> IO ())

data Direction = Forward | Backword

fft1d :: Vector (Complex Double) -> Direction -> Vector (Complex Double)
fft1d vec dir = unsafePerformIO $ do
  let n = V.length vec
  p <- unsafeWith vec $ \v -> do
    case dir of
      Forward -> c_fft1d v (fromIntegral n) 1
      Backword -> c_fft1d v (fromIntegral n) 0
  fp <- newForeignPtr c_p_freeComplexDouble p
  return $ unsafeFromForeignPtr fp 0 n

fft2d :: Vector (Complex Double) -> Int -> Direction -> Vector (Complex Double)
fft2d vec m dir = unsafePerformIO $ do
  let nm = V.length vec
  let n = nm `div` m
  p <- unsafeWith vec $ \v -> do
    case dir of
      Forward -> c_fft2d v (fromIntegral m) (fromIntegral n) 1
      Backword -> c_fft2d v (fromIntegral m) (fromIntegral n) 0
  fp <- newForeignPtr c_p_freeComplexDouble p
  return $ unsafeFromForeignPtr fp 0 nm

fft3d :: Vector (Complex Double) -> Int -> Int -> Direction -> Vector (Complex Double)
fft3d vec l m dir = unsafePerformIO $ do
  let lmn = V.length vec
  let n = lmn `div` (l*m)
  p <- unsafeWith vec $ \v -> do
    case dir of
      Forward -> c_fft3d v (fromIntegral l) (fromIntegral m) (fromIntegral n) 1
      Backword -> c_fft3d v (fromIntegral l) (fromIntegral m) (fromIntegral n) 0
  fp <- newForeignPtr c_p_freeComplexDouble p
  return $ unsafeFromForeignPtr fp 0 lmn

