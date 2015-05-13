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

module Data.Vector.Image.Color.YUV (
  YUV(..)
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

data YUV a = YUV {
   yuv_y :: a
 , yuv_u :: a 
 , yuv_b :: a
} deriving (Show,Eq,Ord)

instance Functor YUV where
  fmap func (YUV r g b) = YUV (func r) (func g) (func b)

rgb2yuv :: (Num a,Integral a) => RGB a -> YUV a
rgb2yuv (RGB r g b) = YUV ((r+2*g+b)`div`4) (b-g) (r-g)

yuv2rgb :: (Num a,Integral a) => YUV a -> RGB a
yuv2rgb (YUV y u v) = RGB (v+g) g (u+g)
  where
    g = y - ((u + v)`div`4)

instance (Num a,Integral a) => ColorSpace YUV a where
  toRGB = yuv2rgb
  fromRGB = rgb2yuv
  toList (YUV r g b) = [r,g,b]
  fromList [r,g,b] = (YUV r g b)
  fromList val = error $ "List length must be 3 but " L.++  show (L.length val)
  dim _ = 3
