{-# LANGUAGE TemplateHaskell #-}

module Gen where

import Text.Cassius
import Data.Text.Lazy (unpack)

main :: IO ()
main = do
    writeFile "default.css" $ unpack $ renderCss def
    writeFile "post.css" $ unpack $ renderCss post

def = $(cassiusFile "default.cassius") ()
post = $(cassiusFile "post.cassius") ()
