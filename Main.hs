{-# LANGUAGE ForeignFunctionInterface #-}

import Data.Vector.Storable
import Foreign
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C.String
import Foreign.C.Types
import GHC.ForeignPtr
import Control.Monad

peekIntConv :: (Storable a, Integral a, Integral b) => Ptr a -> IO b
peekIntConv = liftM fromIntegral . peek

data RGB = RGB {
   rgb_r :: Word8
 , rgb_g :: Word8 
 , rgb_b :: Word8
} deriving (Show)

data Image = 
  Image {
    width :: Int
  , height :: Int
  , dat :: Vector RGB
  }

instance Storable RGB where
  sizeOf    _ = 3
  alignment _ = 3
  peek ptr = do
    r <- peekByteOff ptr 0
    g <- peekByteOff ptr 1
    b <- peekByteOff ptr 2
    return $ RGB r g b
  poke ptr (RGB r g b) = do
    pokeByteOff ptr 0 r
    pokeByteOff ptr 1 g
    pokeByteOff ptr 2 b

readBmp :: String -> IO Image
readBmp file = do
  withCString file $ \ cfile -> do
    alloca $ \cw -> do
      alloca $ \ch -> do
        c_tryReadBmp cfile cw ch
        w <- peekIntConv cw
        h <- peekIntConv ch
        let n = w*h*3
        fp <- mallocPlainForeignPtrBytes n :: IO (ForeignPtr RGB)
        withForeignPtr fp $ \p -> do
          c_readBmp cfile p
        return $ Image w h (unsafeFromForeignPtr fp 0 n)
  where
    doMalloc n dummy = do
        mallocPlainForeignPtrBytes (n * sizeOf dummy)
  
writeBmp :: String -> Image -> IO ()
writeBmp file (Image w h dat) = do
  withCString file $ \ cfile -> do
    unsafeWith dat $ \p -> do
      c_writeBmp cfile p (fromIntegral w) (fromIntegral h)

foreign import ccall unsafe "tryReadBmp" c_tryReadBmp :: Ptr CChar -> Ptr CInt ->  Ptr CInt -> IO ()
foreign import ccall unsafe "tryReadPng" c_tryReadPng :: Ptr CChar -> Ptr CInt ->  Ptr CInt -> IO ()
foreign import ccall unsafe "tryReadJpeg" c_tryReadJpeg :: Ptr CChar -> Ptr CInt ->  Ptr CInt -> IO ()
foreign import ccall unsafe "readBmp" c_readBmp :: Ptr CChar -> Ptr RGB -> IO ()
foreign import ccall unsafe "readPng" c_readPng :: Ptr CChar -> Ptr RGB -> IO ()
foreign import ccall unsafe "readJpeg" c_readJpeg :: Ptr CChar -> Ptr RGB  -> IO ()
foreign import ccall unsafe "writeBmp" c_writeBmp :: Ptr CChar -> Ptr RGB -> CInt -> CInt -> IO ()
foreign import ccall unsafe "writePng" c_writePng :: Ptr CChar -> Ptr RGB -> CInt -> CInt -> IO ()
foreign import ccall unsafe "writeJpeg" c_writeJpeg :: Ptr CChar -> Ptr RGB -> CInt -> CInt -> IO ()

vec w h = Image w h $ fromList $ do
  y <- [0..(h-1)]
  x <- [0..(w-1)]
  let v = fromIntegral $ y * 256 `div` h
  return $ RGB v 0 0


main = do
  writeBmp "hoge.bmp" $ vec 1920 1080
  return ()
