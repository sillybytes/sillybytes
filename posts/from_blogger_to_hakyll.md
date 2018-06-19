---
title: From Blogger to Hakyll
published: 2017-04-10
...

![](/img/bloggerhakyll/thumbnail.png){#thumbnail}\

[Hakyll](https://jaspervdj.be/hakyll/) is an amazing static site generator
written in Haskell, it allows for blog posts to be written in *markdown*, that
are then compiled with *pandoc*, and is very well suited to be used with *GitHub
pages*; It's everything I wanted and more.

[Silly Bytes](http://www.sillybytes.net) went through its first 5 years of
existence hosted on Google's [Blogger](https://www.blogger.com) service, and it
did well. Although Blogger offers a fair amount of flexibility, you can't have
total control over it, and having to write posts with the built in *WYSIWYG*
interface or pasting the HTML output is one of the bigger pain points of it. I
solved most of that by writing a [CLI
tool](http://www.sillybytes.net/2016/09/how-do-i-blog-blogger-posts-from.html)
that allows me to write my posts offline in *markdown*, compile them, and deploy
them from my terminal leveraging the Blogger's convenient API. But I still have
the feeling that it isn't good enough.

In this post I will describe the process of porting an existing Blogger blog to
*Hakyll* and *GitHub pages* using *Silly Bytes* itself as a case study.

<!--more-->

# Expectations

So here is what I want instead:

1. Completely port *Silly Bytes* to *Hakyll* and *GitHub pages*

Write every post in *markdown* only, and have them generated automatically.

2. Further customize the design

While I've managed to get pretty far with Blogger's custom CSS option, there are
still some aspects that doesn't quite fit what I want.

3. Preserve all the links to my previous posts

There are quite a few links to my posts all over the place: Reddit, Taringa,
ElHacker.net, Facebook, etc. I want them to keep working just fine.


# The initial setup

We'll strive to keep the old blog completely functional till the last moment
when we finally change where the domain name points to.

## GitHub page

*GitHub pages* will host the blog, so we must first create a repository that
will keep it.

The *GitHub pages* [naming
convections](https://help.github.com/articles/user-organization-and-project-pages/)
state that, in order to create a dedicated repo for a personal or organizational
page, we must have a repository named `user.github.io` or
`organization.github.io` respectively, this way GitHub will read and serve any
*index* file in the repository root; This supposes a problem though, We want to
keep our generated site inside a directory to keep them isolated from the
sources files.

There are a couple of solutions for this, but they all use some Git branches
trickery, juggling with a CI service, or both; It feels to hacky to me, not to
say that my solution is more elegant, but it just fits better to the work flow
I'm looking for.

*GitHub pages* offers project specific pages as well, those are served from a
dedicated `docs` directory on it, so this is what we're going to use instead.

I've created a `sillybytes`
[repository](https://github.com/sillybytes/sillybytes) in the `sillybytes`
organization. Then in `settings -> GitHub Pages -> Source` I've selected
`master branch /docs folder` as the page source.

## Hakyll site

For the content of that repository, this will create the initial Hakyll
scaffolding:

    $ hakyll-init sillybytes
    $ cd sillybytes
    $ stack init
    $ stack build

By default, Hakyll outputs the generated site in a `_site` directory, but
*GitHub pages* will read the site from a `docs` directory, so let's fix that by
editing the `site.hs` file.

The default `main` function in `site.hs` uses the `hakyll` function, which uses
the default configuration, so we must change that to use a custom one:

```haskell
main = hakyllWith config $ do
    ...
    ...



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
```

Here I've pretty much left the default configuration intact and only changed the
`destinationDirectory` field to be `docs`.

Now recompile and regenerate the site:

    $ stack build
    $ stack exec site rebuild

And the generated site will now be on `docs`.

## Deploying

The deployment process boils down to regenerating the site:

    $ stack exec site rebuild

Committing the changes on `docs`:

    $ git add docs
    $ git commit -m "Build"

And pushing:

    $ git push origin master

No need for esoteric spells here.


# Don't shatter my links!

![](/img/bloggerhakyll/links.png){.img-responsive}

It is imperative to preserve the links to my previous posts that were originally
published on Blogger, so they keep pointing to the right post.

## Preserve legacy paths

Blogger paths convention is as follows:

Every post is on the corresponding *year* and *month* of publication name space
like `year/month/post.html`. So we must preserve this structure at least for the
legacy posts.

In order to achieve this, keep a `legacy` directory inside the `posts`
directory, that will keep a directory tree for every year and month where posts
exist.

```
sillybytes/posts/legacy
|
+---2012
|   |
|   +----01
|   |    +---- post.md
|   |
|   +----02
|   |
|   +---- ...
|
|
+---2013
|   |
|   +----01
|   |
|   +----02
|   |
|   +---- ...
|
|
+--- ...
    |
    +----01
    |
    +----02
    |
    +---- ...
```

Then we need an additional rule in `site.hs`

```haskell
match "posts/legacy/**" $ do
    route $ customRoute $ (flip replaceExtension "html") . joinPath
        . (drop 2) . splitPath . toFilePath
    compile $ pandocCompiler
        >>= saveSnapshot "content"
        >>= loadAndApplyTemplate "templates/post.html"    postCtx
        >>= loadAndApplyTemplate "templates/default.html" postCtx
        >>= relativizeUrls
```

This will ensure that the `year/month/post.html` directory structure is
preserved on the resulting generated site.

## Port legacy posts

From here, a pretty much manual porting process is required. Most of my legacy
posts were originally published right in the Blogger interface, so the must
first be ported to *markdow*.

The porting process is as follows:

1. Visit the legacy post and copy the trailing name of it from the URL.
2. Create the appropriate directory structure inside `posts/legacy` to preserve
    the same `year/month/post.html` path.
3. Create a *markdown* file with the same name as it appears in the URL, but
    with the `.md` extension.
4. Create a dedicated directory for the post inside the `images` directory and
    put all the post images on it.
5. Paste and format the post content in the *markdown* file.

Any newer posts that are created after the porting can live in the `posts`
directory, there is no need to keep the `year/month/post.html` any more.


# The migration

![](/img/bloggerhakyll/migration.jpg){.img-responsive}

The only thing that is left is the actual migration by pointing the domain name
to the new location.

This arises a bigger problem though, given that we are serving the blog from
`sillybytes/docs` we'll need a *URL Redirect* record pointing to
`sillybytes.github.io/sillybytes` rather than a *CNAME* to just
`sillybytes.github.io`. If you're fine with that, then you're done.

I really wanted a proper *CNAME* record, so I had to change the setup a
bit:

* Have two repositories: `sillybytes` for the sources, and
    `sillybytes.github.io` for the generated page.
* A *deployment* consists of copying the content of the `docs` directory to the
    `sillybytes.github.io` repository.
* Point the domain name with a *CNAME* record to `sillybytes.github.io`.


# New CLI tool

![](/img/bloggerhakyll/shot.png){.img-responsive}

The [CLI tool](https://github.com/sillybytes/sillybytes_tool) I was
using before for Blogger deployment is no longer useful, but I can still adapt
it to the new deployment schema:

```sh
cp -rfv _site/* ../sillybytes.github.io/
cd ../sillybytes.github.io
display_info "Deploying..."
git add .
git commit -m "Deploy"
git push origin master
display_success "Deployed!"
```

As well as aliasing common *Hakyll* commands:

![](/img/bloggerhakyll/shot1.png){.img-responsive}
