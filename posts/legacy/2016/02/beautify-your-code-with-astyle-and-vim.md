---
title: Beautify your code with Astyle and Vim
published: 2016-02-26
...

![](/img/astyle/thumbnail.png){#thumbnail}\

Programmers love good code: good expressivity, good organization, ease of
extensibility, maintainability, and so on. But we also like the code to be
**pretty**, it makes the work more pleasant and enjoyable, but all this is
probably not just for visually appealing reasons.

Is always nice to work with a style consistent code, it even makes it easier to
navigate and read, our brains can dissect it more easily and our eyes can
quickly jump to the right place. A pretty code transmits the idea that the
people who wrote it do actually care about the program.

But there are some mundane reasons too... For lots of us an annoying voice in
our heads is constantly telling us "you're wasting storage space, you're and
idiot!" because some trailing white space or unnecessary blank lines are laying
around in the code. Sure, those few extra bytes won't hurt your 1+TB hard
drive!, but for some of us it just feels wrong.

<!--more-->

# Details

There is a lot of stuff that you might want to keep neat in your code:

* Tabs vs Spaces
* Maximum characters length
* Indent style
* Number of spaces per indentation level
* Trailing white space
* Blank lines the end of the file
* Inconsistent number of blink lines between code blocks
* Blank space between functions arguments
* Space padding around operators
* Use and position of brackets
* Blank space between various syntactic characters


# Pretty and pragmatic

As I told before, consistent style in the code help our brains a lot! But there
are actually some extra pragmatic benefits: When using a (good) text editor
(like Vim) you may want to use methods for easy moving thought the code, like
end-of-line or end-of-file (`$`, `G` in Vim), in either case you want to jump to
the last character of the line, not to a white space at the end; and you want to
jump to the last line of code at the end of the file, not a white and useless
line at the end of the file.

A good style consistency can also make some errors pretty obvious using the
pattern recognition capabilities of your brain. But, does all that detail really
matters? Or is just some mental illness programmers develop over time?

Look... The compiler won't complain if you leave ugly white space all over the
place, the control version system will still keep your code, and your tests will
still pass, BUT you and other humans won't be happy working with a messy heap of
characters that nobody cares about, so yeah... It does matters!.


# Beautify

But if you are here, you probably are already aware of all this, so lets just
jump to the useful part.

[Artistic Style - Astyle](http://astyle.sourceforge.net/) is a fantastic
automatic code formatter, so we are going to take advantage of the Unix
philosophy here and use it as the external tool to beautify our code while in
Vim, along with some other things that *Astyle* doesn't handle.

As you might already know, the `=` Vim command allows you to fix the indentation
of the code, so you probably are used to do something like `gg=G` to fix the
entire file.

I prefer not to remap that one as it is useful for visual selection re
indentation, so lets use `g=` to invoke a formatter function. Here it is:

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
you started. Notice it invokes another custom function `RemoveTrailingSpaces()`:

```vim
function! RemoveTrailingSpaces()
    silent! execute '%s/\s\+$//ge'
    silent! execute 'g/\v^$\n*%$/norm! dd'
endfunction
```

So far, so good, but what is *Astyle* actually doing with my code?

We want to control with precision how our code is getting the make up applied,
so we put our preferences in the `~/.astylerc` file using all the options listed
in the *Astyle* [documentation](http://astyle.sourceforge.net/astyle.html). Here
is mine:

* --style=allman
* --indent=spaces -s4
* --indent-classes
* --indent-switches
* --indent-preproc-define
* --indent-col1-comments
* --pad-oper
* --pad-header
* --unpad-paren
* --align-pointer=name
* --add-brackets
* --max-code-length=80


The remarkable thing here is the **allman** style I'm using (and recommend) but
you can use the one that you prefer: k&r, java, bsd, stroustrup, whitesmith,
vtk, gnu, linux, horstmann, and others.

Here you can get my
[.vimrc](https://github.com/alx741/dotfiles/blob/master/nvim/.config/nvim/init.vim)
and my
[.astylerc](https://github.com/alx741/dotfiles/blob/master/astyle/.astylerc)
files.

So you have no excuse now! Trigger that `g=` to keep you code neat and pretty.
