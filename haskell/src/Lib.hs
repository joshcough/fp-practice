{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( someFunc
    ) where

import qualified Data.Text.IO as T
import qualified Text.Trifecta.Parser as P

z = P.manyAccum

someFunc :: IO ()
someFunc = putStrLn "someFunc!"
