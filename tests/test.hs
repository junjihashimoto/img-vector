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
chrome rgb = fmap fromIntegral $ (\(YUV y _ _) -> RGB y y y) $ fromRGB $ (fmap fromIntegral rgb :: RGB Int)


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
    it "toMatrix fromMatrix" $ do
      let org = I.map chrome (vec 100 64)
      fromMatrix (toMatrix org) `shouldBe`  org
    it "toMatrixList fromMatrixList" $ do
      let org = I.map chrome (vec 100 64)
      fromMatrixList (toMatrixList org) `shouldBe`  org
    -- it "toMatrixD fromMatrixD" $ do
    --   let org = I.map chrome (vec 100 64)
    --   fromMatrixD (toMatrixD org) `shouldBe`  org
  describe "ColorSpace test" $ do
    it "rgb+rgb" $ do
      RGB 1 2 3 + RGB 1 2 3 `shouldBe` (RGB 2 4 6  :: RGB Int)
    prop "rgb2yuv yuv2rgb" $ \(v :: RGB Int) -> 
      toRGB (fromRGB v :: YUV Int) == v
