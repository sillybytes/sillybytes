---
title: Beautify your code with Astyle and Vim
published: 2016-02-26
...

Good code is about many things: good expressivity, good organization, ease of
extensibility, maintainability, readability and so on. But we also prefer the
code to be *pretty*, it makes it pleasant and overall more enjoyable to work
with, though is not only for the sake of making it visually appealing. Working
with style-consistent code makes it easier to navigate and read, our brains can
dissect it with less overhead and our eyes can quickly jump to the right place.
Pretty code also conveys that the people who wrote it actually care.

Some things to take care of are:

- Tabs vs Spaces
- Maximum line length
- Indent style
- Number of spaces per indentation level
- Trailing white space
- Blank lines at the end of the file
- Inconsistent number of blink lines between code blocks
- Blank space between function arguments
- Space padding around operators
- Use and position of brackets
- Blank space between various syntactic characters

<!--more-->

# Pretty and pragmatic

A Consistent style helps our brains big time, but there
are actually some extra pragmatic benefits: When using a (good) text editor
(like Vim) you may want to use mechanisms to easily move through the code, like
end-of-line or end-of-file (`$`, `G` in Vim), in either case you want to jump to
the last character of the line, not to a white space at the end, and you want to
jump to the last line of code at the end of the file, not a white and useless
line at the end of the file.

Good style consistency can also make some errors pretty obvious using the visual
pattern recognition capabilities of that brain of yours. But, does all of this
really matter that much? Or is it just some OCD-like mental illness programmers
develop over time?. The compiler won't complain if you leave ugly white space
all over the place, the control version system will still keep your code safe
and sound, and your tests will still pass. You and other fellow human developers
however, won't be as happy working with a messy heap of characters that nobody
cares about, so yeah... It does matter!.


# Beautify

[Artistic Style (Astyle)](http://astyle.sourceforge.net/) is a fantastic,
automatic code formatter, so we are going to take advantage of the Unix
philosophy here and use it as the external tool to beautify our code with while
in Vim, along with some other additional stuff that *Astyle* can't handle.

As you might already know, the `=` Vim command allows you to fix the indentation
of the code, so you are probably used to do something like `gg=G` to fix the
entire file.

I prefer not to remap that one as it is useful for re-indentation with visual
selection. Instead, let's use `g=` to invoke a formatter function:

```vim
function! Format()
    silent! execute 'norm! mz'

    if &ft ==? 'c' || &ft ==? 'cpp' || &ft ==? 'php'
        set formatprg=astyle\ --mode=c
        silent! execute 'norm! gggqG'
    elseif &ft ==? 'java'
        set formatprg=astyle\ --mode=java
        silent! execute 'norm! gggqG'
    endif

    silent! call RemoveTrailingSpaces()
    silent! execute 'retab'
    silent! execute 'gg=G'
    silent! execute 'norm! `z'
    set formatprg=
endfunction
```

This will invoke *Astyle* and set the `--mode` option depending on the current
*filetype*, then remove the trailing spaces, remove blank lines at the end of
the file, remove all the tab characters and replace them with proper space
indentation, fix the indentation of the whole file and finally leave you where
you started. Notice that it invokes another custom function
`RemoveTrailingSpaces()`:

```vim
function! RemoveTrailingSpaces()
    silent! execute '%s/\s\+$//ge'
    silent! execute 'g/\v^$\n*%$/norm! dd'
endfunction
```

So far so good, but what's *Astyle* actually doing with the code?

We want to be able to control with precision how the code is sculpted, so let's
make sure our preferences are laid out in the `~/.astylerc` file, using all the
options listed in [Astyle's
documentation](http://astyle.sourceforge.net/astyle.html). Here are mine:

- --style=allman
- --indent=spaces -s4
- --indent-classes
- --indent-switches
- --indent-preproc-define
- --indent-col1-comments
- --pad-oper
- --pad-header
- --unpad-paren
- --align-pointer=name
- --add-brackets
- --max-code-length=80


The most important part here is the *allman* style I'm using (and recommend) but
you can use the one that you prefer: k&r, java, bsd, stroustrup, whitesmith,
vtk, gnu, linux, horstmann, and others.

Here are my [.vimrc](https://github.com/alx741/dotfiles/blob/master/vim/.vimrc)
and [.astylerc](https://github.com/alx741/dotfiles/blob/master/astyle/.astylerc)
files for reference.

So you have no excuse now. Trigger that `g=` and keep you code neat and pretty.
