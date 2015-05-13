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

module Data.Vector.Image (
  RGB(..)
, YUV(..)
, Image(..)
, ColorSpace(..)
, RGB8
, Image8
, map
, (!)
, (!?)
, createImg
, createImgI
, readImg
, writeImg
, writeBmp
, writePng
, writeJpeg
, toMatrix
, fromMatrix
, toMatrixList
, fromMatrixList
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
  

data YUV a = YUV {
   yuv_y :: a
 , yuv_u :: a 
 , yuv_b :: a
} deriving (Show,Eq,Ord)

instance Functor YUV where
  fmap func (YUV r g b) = YUV (func r) (func g) (func b)


type RGB8 = RGB Word8

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


rgb2yuv :: (Num a,Integral a) => RGB a -> YUV a
rgb2yuv (RGB r g b) = YUV ((r+2*g+b)`div`4) (b-g) (r-g)

yuv2rgb :: (Num a,Integral a) => YUV a -> RGB a
yuv2rgb (YUV y u v) = RGB (v+g) g (u+g)
  where
    g = y - ((u + v)`div`4)

instance ColorSpace RGB a where
  toRGB = id
  fromRGB = id
  toList (RGB r g b) = [r,g,b]
  fromList [r,g,b] = (RGB r g b)
  fromList val = error $ "List length must be 3 but " L.++  show (L.length val)
  dim _ = 3

instance (Num a,Integral a) => ColorSpace YUV a where
  toRGB = yuv2rgb
  fromRGB = rgb2yuv
  toList (YUV r g b) = [r,g,b]
  fromList [r,g,b] = (YUV r g b)
  fromList val = error $ "List length must be 3 but " L.++  show (L.length val)
  dim _ = 3


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

type WriteImgFunc = Ptr CChar -- ^ Filename
                 -> Ptr RGB8  -- ^ Image Vector
                 -> CInt      -- ^ Width of Image
                 -> CInt      -- ^ Height of Image
                 -> IO CInt   -- ^ When write fails, this returns negative value. When success, this returns 0.

foreign import ccall unsafe "readImg" c_readImg :: Ptr CChar -> Ptr (Ptr RGB8) -> Ptr CInt -> Ptr CInt -> IO CInt
foreign import ccall unsafe "&freeImg" c_p_freeImg :: FunPtr(Ptr RGB8 -> IO ())
foreign import ccall unsafe "writeBmp" c_writeBmp :: WriteImgFunc
foreign import ccall unsafe "writePng" c_writePng :: WriteImgFunc
foreign import ccall unsafe "writeJpeg" c_writeJpeg :: WriteImgFunc

readImg :: String  -- ^ Filename
        -> IO (Either Int Image8)
readImg file = do
  withCString file $ \ cfile -> do
    alloca $ \cp -> do
      alloca $ \cw -> do
        alloca $ \ch -> do
          r <- fmap fromIntegral $ c_readImg cfile cp cw ch
          w <- fmap fromIntegral $ peek cw
          h <- fmap fromIntegral $ peek ch
          p <- peek cp :: IO (Ptr RGB8)
          let n = w*h
          fp <- newForeignPtr c_p_freeImg p
          return $ if r < 0
                   then Left r
                   else Right $  Image w h (unsafeFromForeignPtr fp 0 n)

writeImg' :: WriteImgFunc -> String -> Image8 -> IO (Either Int ())
writeImg' func file (Image w h dat) = do
  r <- fmap fromIntegral $ withCString file $ \ cfile -> do
    unsafeWith dat $ \p -> do
      func cfile p (fromIntegral w) (fromIntegral h)
  return $ if r < 0 then Left r else Right ()


writeImg :: String   -- ^ Filename
         -> Image8  -- ^ Image Data
         -> IO (Either Int ())
writeImg file img | L.isSuffixOf ".bmp" file = writeBmp file img
                  | L.isSuffixOf ".BMP" file = writeBmp file img
                  | L.isSuffixOf ".png" file = writePng file img
                  | L.isSuffixOf ".Png" file = writePng file img
                  | L.isSuffixOf ".jpg" file = writeJpeg file img
                  | L.isSuffixOf ".JPG" file = writeJpeg file img
                  | otherwise = return $ Left (-1)


writeBmp :: String   -- ^ Filename
         -> Image8  -- ^ Image Data
         -> IO (Either Int ())
writeBmp = writeImg' c_writeBmp

writeJpeg :: String  -- ^ Filename
          -> Image8 -- ^ Image Data
          -> IO (Either Int ())
writeJpeg = writeImg' c_writeJpeg

writePng :: String  -- ^ Filename
         -> Image8 -- ^ Image Data
         ->  IO (Either Int ())
writePng = writeImg' c_writePng

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
