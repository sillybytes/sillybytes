{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Data.Monoid
import GHC.Generics
import Hakyll
import Data.List (isPrefixOf, isSuffixOf)
import Data.Monoid
import System.FilePath (takeFileName, splitPath, joinPath, replaceExtension)
import System.Process (system)
import Data.ByteString.Lazy (ByteString)
import Data.Aeson
import Debug.Trace

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

    match "js/**" $ do
        route   idRoute
        compile copyFileCompiler

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
        route $ customRoute $ flip replaceExtension "html" . joinPath
            . drop 2 . splitPath . toFilePath
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

    create ["atom.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx = postCtx <> bodyField "description"
            posts <- fmap (take 10) . recentFirst =<<
                loadAllSnapshots "posts/**" "content"
            renderAtom feedConfig feedCtx posts


    create ["postsList.json"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAllSnapshots "posts/**" "content"
            postsListCompiler posts postCtx


    match "templates/*" $ compile templateBodyCompiler

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" <>
    constField "author" "Daniel Campoverde" <>
    defaultContext

teaserCtx :: Context String
teaserCtx = teaserField "teaser" "content" <> postCtx


feedConfig :: FeedConfiguration
feedConfig = FeedConfiguration
    { feedTitle       = "Silly Bytes"
    , feedDescription = "Silly Bytes blog posts"
    , feedAuthorName  = "Daniel Campoverde [alx741]"
    , feedAuthorEmail = "alx@sillybytes.net"
    , feedRoot        = "http://www.sillybytes.net"
    }


data Post = Post { title :: String, url :: String} deriving Generic

instance ToJSON Post where
    toEncoding = genericToEncoding defaultOptions

postsListCompiler :: [Item String] -> Context String -> Compiler (Item ByteString)
postsListCompiler posts ctx = do
    posts' <- fmap encode $ sequence $ fmap (getPost ctx) posts
    makeItem posts'
    where
        getPost :: Context String -> Item String -> Compiler Post
        getPost ctx itemPost = Post
            <$> getField "title" ctx itemPost
            <*> getField "url" ctx itemPost

        getField :: String -> Context String -> Item String -> Compiler String
        getField field ctx post = do
            field <- unContext ctx field [] post
            case field of
                StringField a -> return a
                _ -> error "Unsoported postsList field"
