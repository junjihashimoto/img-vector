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
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Vector.Image.Image (
  Image(..)
, Image8
, map
, (!)
, (!?)
, createImg
, createImgI
, splitColor
, mergeColor
, toDouble
, fromDouble
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


data Image a = 
  Image {
    img_width :: Int
  , img_height :: Int
  , img_dat :: Vector a
  } deriving (Show,Eq)

type Image8 = Image (RGB Word8)

map :: (Storable a, Storable b) => (a->b) -> Image a -> Image b
map func (Image w h d) = (Image w h (V.map func d))

index :: Storable a => Image a -> (Int,Int) -> a
index (Image w h d) (x,y) = d V.! (w*y+x)

(!?) :: Storable a => Image a -> (Int,Int) -> Maybe a
(!?) img@(Image w h d) (x,y) | x < 0 || x >= w || y <0 || y>= h = Nothing
                             | otherwise = Just (index img (x,y))

(!) :: (Storable a,Integral a) => Image a -> (Int,Int) -> a
(!) img idx = fromMaybe 0 (img !? idx)



-- | Generate Image
createImg :: Int -- ^ Width of image
            -> Int -- ^ Height of image
            -> (Int -> Int -> RGB8) -- ^ Pixel Generator
            -> Image8
createImg w h func = Image w h $ V.fromList $ do
  y <- [0..(h-1)]
  x <- [0..(w-1)]
  return $ func x y

-- | Generate Image
createImgI :: Int -- ^ Width of image
           -> Int -- ^ Height of image
           -> (Int -> Int -> RGB Int) -- ^ Pixel Generator
           -> Image8
createImgI w h func = Image w h $ V.fromList $ do
  y <- [0..(h-1)]
  x <- [0..(w-1)]
  return $ fmap fromIntegral $ fmap (\v -> if v < 0 then 0 else if v > 255 then 255 else v) $ func x y

splitColor :: (ColorSpace c a,Storable (c a),Storable a)
           => Image (c a)
           -> [Image a]
splitColor img@(Image w h d) = flip P.map [0..(num-1)] $ (\idx -> Image w h $ V.map (\v -> toList v !! idx) d)
  where
    num :: Int
    num = dim (dummyColor img)
    dummyColor :: Image (c a) -> c a
    dummyColor = error "do not eval this"

mergeColor :: (ColorSpace c a,Storable (c a),Storable a,Element a)
              => [Image a]
              -> Image (c a)
mergeColor img@((Image w h d):_) = Image w h $ V.fromList $ P.map fromList $ L.transpose $ P.map (V.toList.img_dat) img
mergeColor [] = error "image-list is null."

toDouble :: (Integral a,Storable a) => Image a -> Image Double
toDouble img= map fromIntegral img

fromDouble :: (Integral a,Storable a) => Image Double -> Image a
fromDouble img = map truncate img
                           
