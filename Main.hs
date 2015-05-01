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

import Data.Vector.Storable hiding(forM_)
import Foreign
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C.String
import Foreign.C.Types
import GHC.ForeignPtr
import Control.Monad

data RGB a = RGB {
   rgb_r :: a
 , rgb_g :: a 
 , rgb_b :: a
} deriving (Show,Eq)

instance Functor RGB where
  fmap func (RGB r g b) = RGB (func r) (func g) (func b)

data YUV a = YUV {
   yuv_y :: a
 , yuv_u :: a 
 , yuv_b :: a
} deriving (Show,Eq)

instance Functor YUV where
  fmap func (YUV r g b) = YUV (func r) (func g) (func b)

type RGB8 = RGB Word8

data Image a = 
  Image {
    width :: Int
  , height :: Int
  , dat :: Vector (RGB a)
  } deriving (Show,Eq)

type Image8 = Image Word8

instance Storable a => Storable (RGB a) where
  sizeOf    _ = 3 * sizeOf (error "do not eval this" :: a)
  alignment _ = 3 * sizeOf (error "do not eval this" :: a)
  peek ptr = do
    r <- peekByteOff ptr 0
    g <- peekByteOff ptr (sizeOf (error "do not eval this" :: a))
    b <- peekByteOff ptr (2 * sizeOf (error "do not eval this" :: a))
    return $ RGB r g b
  poke ptr (RGB r g b) = do
    pokeByteOff ptr 0 r
    pokeByteOff ptr (sizeOf (error "do not eval this" :: a)) g
    pokeByteOff ptr (2 * sizeOf (error "do not eval this" :: a)) b

instance Storable a => Storable (YUV a) where
  sizeOf    _ = 3 * sizeOf (error "do not eval this" :: a)
  alignment _ = 3 * sizeOf (error "do not eval this" :: a)
  peek ptr = do
    r <- peekByteOff ptr 0
    g <- peekByteOff ptr (sizeOf (error "do not eval this" :: a))
    b <- peekByteOff ptr (2 * sizeOf (error "do not eval this" :: a))
    return $ RGB r g b
  poke ptr (RGB r g b) = do
    pokeByteOff ptr 0 r
    pokeByteOff ptr (sizeOf (error "do not eval this" :: a)) g
    pokeByteOff ptr (2 * sizeOf (error "do not eval this" :: a)) b

rgb2yuv :: (Num a,Integral a) => RGB a -> YUV a
rgb2yuv (RGB r g b) = YUV ((r+2*g+b)`div`4) (b-g) (r-g)
yuv2rgb :: (Num a,Integral a) => YUV a -> RGB a
yuv2rgb (YUV y u v) = RGB (u+g) g (v+u)
  where
    g = ((y-(u+v)`div`2)`div`2)

readImg :: String -> IO Image8
readImg file = do
  withCString file $ \ cfile -> do
    alloca $ \cp -> do
      alloca $ \cw -> do
        alloca $ \ch -> do
          c_readImg cfile cp cw ch
          w <- fmap fromIntegral $ peek cw
          h <- fmap fromIntegral $ peek ch
          p <- peek cp :: IO (Ptr RGB8)
          let n = w*h
          fp <- newForeignPtr c_p_freeImg p
          return $ Image w h (unsafeFromForeignPtr fp 0 n)
  where
    doMalloc n dummy = do
        mallocPlainForeignPtrBytes (n * sizeOf dummy)
  
writeBmp :: String -> Image8 -> IO Int
writeBmp file (Image w h dat) = do
  withCString file $ \ cfile -> do
    unsafeWith dat $ \p -> do
      fmap fromIntegral $ c_writeBmp cfile p (fromIntegral w) (fromIntegral h)

writeJpeg :: String -> Image8 -> IO Int
writeJpeg file (Image w h dat) = do
  withCString file $ \ cfile -> do
    unsafeWith dat $ \p -> do
      fmap fromIntegral $ c_writeJpeg cfile p (fromIntegral w) (fromIntegral h)

writePng :: String -> Image8 -> IO Int
writePng file (Image w h dat) = do
  withCString file $ \ cfile -> do
    unsafeWith dat $ \p -> do
      fmap fromIntegral $ c_writePng cfile p (fromIntegral w) (fromIntegral h)

foreign import ccall unsafe "readImg" c_readImg :: Ptr CChar -> Ptr (Ptr RGB8) -> Ptr CInt -> Ptr CInt -> IO CInt
foreign import ccall unsafe "&freeImg" c_p_freeImg :: FunPtr(Ptr RGB8 -> IO ())
foreign import ccall unsafe "writeBmp" c_writeBmp :: Ptr CChar -> Ptr RGB8 -> CInt -> CInt -> IO CInt
foreign import ccall unsafe "writePng" c_writePng :: Ptr CChar -> Ptr RGB8 -> CInt -> CInt -> IO CInt
foreign import ccall unsafe "writeJpeg" c_writeJpeg :: Ptr CChar -> Ptr RGB8 -> CInt -> CInt -> IO CInt

vec :: Int -> Int -> Image8
vec w h = Image w h $ fromList $ do
  y <- [0..(h-1)]
  x <- [0..(w-1)]
  let v = fromIntegral $ y * 256 `div` h
  return $ RGB v 0 0

main = do
  writeBmp "hoge.bmp" $ vec 1920 1080
  writeJpeg "hoge.jpg" $ vec 1920 1080
  writePng "hoge.png" $ vec 1920 1080
  img0 <- readImg "hoge.bmp"
  img1 <- readImg "hoge.jpg"
  img2 <- readImg "hoge.png"
  writeBmp "hoge2.bmp" $ img0
  writeJpeg "hoge2.jpg" $ img1
  writePng "hoge2.png" $ img2
  return ()
