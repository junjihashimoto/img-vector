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
} deriving (Show,Eq)

data Image = 
  Image {
    width :: Int
  , height :: Int
  , dat :: Vector RGB
  } deriving (Show,Eq)

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

readImg :: String -> IO Image
readImg file = do
  withCString file $ \ cfile -> do
    alloca $ \cp -> do
      alloca $ \cw -> do
        alloca $ \ch -> do
          c_readImg cfile cp cw ch
          w <- fmap fromIntegral $ peek cw
          h <- fmap fromIntegral $ peek ch
          p <- peek cp
          let n = w*h
          fp <- newForeignPtr c_p_freeImg p
          return $ Image w h (unsafeFromForeignPtr fp 0 n)
  where
    doMalloc n dummy = do
        mallocPlainForeignPtrBytes (n * sizeOf dummy)
  
writeBmp :: String -> Image -> IO Int
writeBmp file (Image w h dat) = do
  withCString file $ \ cfile -> do
    unsafeWith dat $ \p -> do
      fmap fromIntegral $ c_writeBmp cfile p (fromIntegral w) (fromIntegral h)

writeJpeg :: String -> Image -> IO Int
writeJpeg file (Image w h dat) = do
  withCString file $ \ cfile -> do
    unsafeWith dat $ \p -> do
      fmap fromIntegral $ c_writeJpeg cfile p (fromIntegral w) (fromIntegral h)

writePng :: String -> Image -> IO Int
writePng file (Image w h dat) = do
  withCString file $ \ cfile -> do
    unsafeWith dat $ \p -> do
      fmap fromIntegral $ c_writePng cfile p (fromIntegral w) (fromIntegral h)

foreign import ccall unsafe "readImg" c_readImg :: Ptr CChar -> Ptr (Ptr RGB) -> Ptr CInt -> Ptr CInt -> IO CInt
foreign import ccall unsafe "&freeImg" c_p_freeImg :: FunPtr(Ptr RGB -> IO ())
foreign import ccall unsafe "writeBmp" c_writeBmp :: Ptr CChar -> Ptr RGB -> CInt -> CInt -> IO CInt
foreign import ccall unsafe "writePng" c_writePng :: Ptr CChar -> Ptr RGB -> CInt -> CInt -> IO CInt
foreign import ccall unsafe "writeJpeg" c_writeJpeg :: Ptr CChar -> Ptr RGB -> CInt -> CInt -> IO CInt

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
