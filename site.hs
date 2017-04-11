{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid (mappend)
import Hakyll
import Data.List (isPrefixOf, isSuffixOf)
import Data.Monoid
import System.FilePath (takeFileName, splitPath, joinPath, replaceExtension)
import System.Process (system)

main :: IO ()
main = hakyll $ do
    match "CNAME" $ do
        route   idRoute
        compile copyFileCompiler

    match "img/**" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*.css" $ do
        route   idRoute
        compile compressCssCompiler

    match "bower_components/**" $ do
        route   idRoute
        compile copyFileCompiler

    match "posts/*.md" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    match "drafts/*.md" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    match "posts/legacy/**" $ do
        route $ customRoute $ (flip replaceExtension "html") . joinPath
            . (drop 2) . splitPath . toFilePath
        compile $ pandocCompiler
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAllSnapshots "posts/**" "content"
            let archiveCtx =
                    listField "posts" teaserCtx (return posts) <>
                    constField "title" "Archives"            <>
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- fmap (take 10) $ recentFirst =<< loadAllSnapshots "posts/**" "content"
            let indexCtx =
                    listField "posts" teaserCtx (return posts) <>
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls


    match "drafts.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAllSnapshots "drafts/**" "content"
            let indexCtx =
                    listField "posts" teaserCtx (return posts) <>
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler


postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" <>
    constField "author" "Daniel Campoverde" <>
    defaultContext

teaserCtx :: Context String
teaserCtx = teaserField "teaser" "content" <> postCtx

config :: Configuration
config = Configuration
    { destinationDirectory = "docs"
    , storeDirectory       = "_cache"
    , tmpDirectory         = "_cache/tmp"
    , providerDirectory    = "."
    , ignoreFile           = ignoreFile'
    , deployCommand        = "echo 'No deploy command specified' && exit 1"
    , deploySite           = system . deployCommand
    , inMemoryCache        = True
    , previewHost          = "127.0.0.1"
    , previewPort          = 8000
    }
  where
    ignoreFile' path
        | "."    `isPrefixOf` fileName = True
        | "#"    `isPrefixOf` fileName = True
        | "~"    `isSuffixOf` fileName = True
        | ".swp" `isSuffixOf` fileName = True
        | otherwise                    = False
      where
        fileName = takeFileName path
