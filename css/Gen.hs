{-# LANGUAGE TemplateHaskell #-}

module Gen where

import Text.Cassius
import Data.Text.Lazy (unpack)

main :: IO ()
main = do
    writeFile "post.css" $ unpack $ renderCss post
    -- writeFile "some.css" $ unpack $ renderCss some

post = $(cassiusFile "post.cassius") ()
-- some = $(cassiusFile "some.cassius") ()
