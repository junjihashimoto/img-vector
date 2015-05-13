{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Prelude hiding (map)
import Data.Vector.Image
import Options.Applicative

import Control.Monad

data Command
  = Sharp FilePath FilePath
  | Mean FilePath FilePath
  deriving Show

sharp :: Parser Command
sharp = Sharp <$> argument str (metavar "INPUT-FILE")
              <*> argument str (metavar "INPUT-FILE")

mean :: Parser Command
mean = Mean <$> argument str (metavar "INPUT-FILE")
            <*> argument str (metavar "INPUT-FILE")


parse :: Parser Command
parse = subparser $ command "sharp" (info sharp (progDesc "sharpness filter")) 
                 <> command "mean"  (info mean (progDesc "mean filter"))

runCmd :: Command -> IO ()
runCmd (Sharp inf outf) = do
  eimg  <- readImg inf
  case eimg of
    Left error -> fail $ show error
    Right img'@(Image w h _) -> do
      let img = map (fmap fromIntegral) img'
      _ <- writeImg outf $ createImgI w h $ \ x y ->
        (                    (-1) * (img!(x,y-1)) +
         (-1) * (img!(x-1,y)) + 8 * (img!(x,y)) + (-1) * (img!(x+1,y)) +
                             (-1) * (img!(x,y+1))
        ) `div` 4
      return ()

runCmd (Mean inf outf) = do
  eimg  <- readImg inf
  case eimg of
    Left error -> fail $ show error
    Right img'@(Image w h _) -> do
      let img = map (fmap fromIntegral) img'
      _ <- writeImg outf $ createImgI w h $ \ x y ->
        (                      (img ! (x,y-1)) +
         (img ! (x-1,y)) + 2 * (img ! (x,y)) + (img ! (x+1,y)) +
                               (img ! (x,y+1))
        ) `div` 6
      return ()


opts :: ParserInfo Command
opts = info (parse <**> helper) idm

main :: IO ()
main = execParser opts >>= runCmd
