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

module Data.Vector.Image.Matrix (
  toMatrix
, fromMatrix
, toMatrixList
, fromMatrixList
, toMatrixD
, fromMatrixD
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
import Data.Vector.Image.Image

toMatrix :: (Storable a)
        => Image a
        -> Matrix a
toMatrix (Image w _h v) = reshape w v

fromMatrix :: (Element a)
        => Matrix a
        -> Image a
fromMatrix mat = Image (cols mat) (rows mat) (flatten mat)

toMatrixList :: (ColorSpace c a,Storable (c a),Storable a,Element a)
              => Image (c a)
              -> [Matrix a]
toMatrixList img = P.map toMatrix $ splitColor img

fromMatrixList :: (ColorSpace c a,Storable (c a),Storable a,Element a)
              => [Matrix a]
              -> Image (c a)
fromMatrixList mats = mergeColor $ P.map fromMatrix mats


toMatrixD :: (ColorSpace c a,Storable (c a),Storable a,Element a,Integral a)
         => Image (c a)
         -> [Matrix Double]
toMatrixD img = P.map toMatrix $ P.map toDouble $ splitColor img

fromMatrixD :: (ColorSpace c a,Storable (c a),Storable a,Element a,Integral a)
           => [Matrix Double]
           -> Image (c a)
fromMatrixD mats = mergeColor $ P.map fromDouble $ P.map fromMatrix mats
