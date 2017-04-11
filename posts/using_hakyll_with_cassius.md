---
title: Using Cassius (Shakespearean template) with Hakyll
published: 2017-04-11
...

![](/img/hakyllcassius/thumbnail.png){#thumbnail}\

As a user of the [Yesod](http://www.yesodweb.com/) framework, I've came to know
and love the
[Cassius](https://www.stackage.org/haddock/lts-8.4/shakespeare-2.0.12.1/Text-Cassius.html#v:cassius)
CSS templating language, although its reliance on Template Haskell is meant to
fit better with the Yesod's needs and makes it a bit cumbersome to use
everywhere else, I still like the templating language itself and its features
lot.

That's why I used it for the styles generation for [Silly
Bytes](http://www.sillybytes.net) together with
[Hakyll](https://jaspervdj.be/hakyll/). In this post I will describe the
process.
\
\
\

<!--more-->

# Cassius files

Our `.cassius` files will live inside the `css` directory, together with a
`Gen.hs` Haskell module that will take the *Cassius* sources and compile them to
*CSS*:

```haskell
{-# LANGUAGE TemplateHaskell #-}

module Gen where

import Text.Cassius
import Data.Text.Lazy (unpack)

main :: IO ()
main = do
    writeFile "default.css" $ unpack $ renderCss def
    writeFile "post.css" $ unpack $ renderCss post
    writeFile "post-list.css" $ unpack $ renderCss postList

def = $(cassiusFile "default.cassius") ()
post = $(cassiusFile "post.cassius") ()
postList = $(cassiusFile "post-list.cassius") ()
```

This module, when executed (`runhaskell Gen.hs`), will compile the *Cassius*
sources `default.cassius`, `post.cassius` and `post-list.cassius` to the
corresponding *CSS* files that the -untouched- *CSS* rule in `site.hs` will
take and use in the generated site.

# Compiling

The *Cassius* compilation doesn't happen when we `stack exec site build`, as we
haven't defined a rule, nor a compiler for them in `site.hs` and we won't,
because the Template Haskell requirements mess things up.

So instead we are going to have a `Makefile` that will watch for changes in all
the `css/*.cassius` files and perform the recompilation by executing `Gen.hs`:

```make
.PHONY: build test css

build: css
    stack build
	stack exec site rebuild

css:
	cd css && stack runhaskell Gen.hs

watch:
	while true; do make css; inotifywait -qre close_write css/*.cassius; done
```

This way, we can execute `make watch` and it will recompile the *Cassius* files
when needed. A normal `stack exec site watch` can be running along the side to
take care of everything else.
