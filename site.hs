{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Data.Monoid
import Control.Monad
import GHC.Generics
import Hakyll
import Data.List (isPrefixOf, isSuffixOf)
import Data.Monoid
import System.FilePath (takeFileName, splitPath, joinPath, replaceExtension)
import System.Process (system)
import Data.ByteString.Lazy (ByteString)
import Data.Aeson

main :: IO ()
main = hakyll $ do
    match "img/**" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*.css" $ do
        route   idRoute
        compile compressCssCompiler

    match "public_key.asc" $ do
        route   idRoute
        compile copyFileCompiler

    match "daniel_campoverde-cv.pdf" $ do
        route   idRoute
        compile copyFileCompiler

    match "about.md" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/about.html"   postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

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


    paginate <- buildPaginateWith pagesGrouper "posts/**" makePageId
    paginateRules paginate $ \page pattern -> do
        route idRoute
        compile $ do
            posts <- recentFirst =<< (loadAllSnapshots pattern "content" :: Compiler [Item String])
            let indexCtx =
                    listField "posts" teaserCtx (return posts) <>
                    paginateContext paginate page <>
                    defaultContext

            makeItem ""
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/post-list.html" indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls


    create ["atom.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx = postCtx <> bodyField "description"
            posts <- fmap (take 15) . recentFirst =<<
                loadAllSnapshots "posts/**" "content"
            renderAtom feedConfig feedCtx posts


    create ["postsList.json"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAllSnapshots "posts/**" "content"
            postsListCompiler posts postCtx


    match "templates/*" $ compile templateBodyCompiler


----------
-- Context
----------

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" <>
    constField "author" "Daniel Campoverde" <>
    defaultContext

teaserCtx :: Context String
teaserCtx = teaserField "teaser" "content" <> postCtx


--------
-- Feeds
--------

feedConfig :: FeedConfiguration
feedConfig = FeedConfiguration
    { feedTitle       = "Silly Bytes"
    , feedDescription = "Silly Bytes blog posts"
    , feedAuthorName  = "Daniel Campoverde [alx741]"
    , feedAuthorEmail = "alx@sillybytes.net"
    , feedRoot        = "http://www.sillybytes.net"
    }


-----------------
-- postsList.json
-----------------

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


-------------
-- Pagination
-------------

makePageId :: PageNumber -> Identifier
makePageId n = fromFilePath $ case n of
        1 -> "index.html"
        _ -> show n ++ "/index.html"

pagesGrouper :: (MonadFail m, MonadMetadata m) => [Identifier] -> m [[Identifier]]
pagesGrouper = liftM (paginateEvery 15) . sortRecentFirst
