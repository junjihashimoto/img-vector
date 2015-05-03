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
import qualified Data.Vector.Image as I
import Data.Vector.Image
import Data.Word
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Arbitrary

instance Arbitrary (RGB Int) where
  arbitrary = do
    r <- arbitrary
    g <- arbitrary
    b <- arbitrary
    return $ RGB r g b

vec :: Int -> Int -> Image8
vec w h = createImg w h $ \_ y -> 
  let v = fromIntegral $ y * 256 `div` h
  in RGB v 0 0

chrome :: RGB Word8 -> RGB Word8
chrome rgb = fmap fromIntegral $ (\(YUV y _ _) -> RGB y y y) $ rgb2yuv $ (fmap fromIntegral rgb :: RGB Int)



main :: IO ()
main = hspec $ do
  describe "IO test" $ do
    it "write and read bmp" $ do
      let org = (vec 100 64)
      writeBmp "hoge.bmp" org `shouldReturn` Right ()
      readImg "hoge.bmp" `shouldReturn` Right org
    it "write and read png" $ do
      let org = (vec 100 64)
      writePng "hoge.png" org `shouldReturn` Right ()
      readImg "hoge.png" `shouldReturn` Right org
    it "map" $ do
      let org = I.map chrome (vec 100 64)
      writePng "hoge2.png" org `shouldReturn` Right ()
    prop "rgb2yuv yuv2rgb" $ \(v :: RGB Int) -> 
      yuv2rgb (rgb2yuv v) == v
