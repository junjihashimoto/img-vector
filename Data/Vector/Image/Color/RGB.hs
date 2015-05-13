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

module Data.Vector.Image.Color.RGB (
  RGB(..)
, ColorSpace(..)
, RGB8
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


data RGB a = RGB {
   rgb_r :: a
 , rgb_g :: a 
 , rgb_b :: a
} deriving (Show,Eq,Ord)

type RGB8 = RGB Word8

class ColorSpace color b where
  toRGB :: color b -> RGB b
  fromRGB :: RGB b -> color b
  toList :: color b -> [b]
  fromList :: [b] -> color b
  dim :: color b -> Int
  dummyElem :: color b -> b
  dummyElem _ = error "do not eval this"

instance Functor RGB where
  fmap func (RGB r g b) = RGB (func r) (func g) (func b)

op :: (ColorSpace c a,Num a) => (a -> a -> a) -> c a -> c a -> c a
op func rgb rgb2 = 
  let rgb' = toList rgb
      rgb2' = toList rgb2
      in fromList $ P.map (uncurry func) $ zip rgb' rgb2'


fmap' :: (ColorSpace c a, ColorSpace c b)
     => (a -> b)
     -> c a
     -> c b
fmap' func val = fromList $ P.map func $ toList val

instance (ColorSpace c a,Num a) => Num (c a) where
  (+) = op (+)
  (-) = op (-) 
  (*) = op (*)
  negate = fmap' negate
  abs = fmap' abs
  signum = fmap' signum
  fromInteger v = fromList $ replicate (dim dummyColor) v'
    where
      v' = fromIntegral v
      dummyColor = error "do not eval this" :: (c a)

instance Enum (RGB Int) where
  succ rgb =  rgb + RGB 1 1 1
  pred rgb =  rgb - RGB 1 1 1
  toEnum i = fromIntegral i
  fromEnum (RGB r g b) = r
  enumFrom rgb = rgb : (enumFrom $ succ rgb)
  enumFromThen rgb rgb1 = rgb : (enumFromThen rgb1 (2 * rgb1 - rgb))

instance Real (RGB Int) where
  toRational (RGB r g b) = (fromIntegral $ (r + 2*g + b) `div` 4) % 1
  
instance Integral (RGB Int) where
  quot = op quot
  rem = op rem
  div = op div
  mod = op mod
  quotRem a b = (quot a b,rem a b)
  divMod a b = (div a b,mod a b)
  toInteger (RGB r g b) = fromIntegral $ (r + 2*g + b) `div` 4
  

instance (Storable a) => Element a
instance (ColorSpace c a,Storable a) => Element (c a)

instance (ColorSpace c a,Storable a) => Storable (c a) where
  sizeOf    d = dim d * sizeOf (dummyElem d)
  alignment d = dim d * sizeOf (dummyElem d)
  peek ptr = do
    v <- C.forM [0..((dim d)-1)] $ \idx -> do
      peekByteOff ptr (sizeOf (dummyElem d) * idx)
    return $ fromList v
    where
      d = error "do not eval this" :: c a
  poke ptr val = do
    C.forM_ (zip [0..] (toList val)) $ \(idx,v) -> do
      pokeByteOff ptr (sizeOf (dummyElem d) * idx) v
    where
      d = error "do not eval this" :: c a

instance ColorSpace RGB a where
  toRGB = id
  fromRGB = id
  toList (RGB r g b) = [r,g,b]
  fromList [r,g,b] = (RGB r g b)
  fromList val = error $ "List length must be 3 but " L.++  show (L.length val)
  dim _ = 3

