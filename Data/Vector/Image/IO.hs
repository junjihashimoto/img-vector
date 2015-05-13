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

module Data.Vector.Image.IO (
  readImg
, writeImg
, writeBmp
, writePng
, writeJpeg
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
