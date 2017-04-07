---
title: How to write C in 2016
published: 2016-06-19
...

![](/img/clang/thumbnail.png){#thumbnail}\

Matt wrote a very interesting and totally recommended post about [how to C (as
of 2016)](https://matt.sh/howto-c). Keith Thompson wrote later a very detailed
and rather useful
[critique](https://github.com/Keith-S-Thompson/how-to-c-response/blob/master/README.md)
with some extra notes about Matt's post.

Go a head and read both articles right now!

Here I would like to point out some things about tooling.

![](/img/clang/shot1.jpg){.img-responsive}
<!--more-->

# Use building tools (Make, Autotools, CMake)

![](/img/clang/shot2.png){.img-responsive}

This might be obvious for most C programmers, but I've seen quite a lot of
people, novices specially, copying and pasting the compilation command on each
iteration.

Using a build tool will allow you to automate the building process, but also
testing, distribution package generation, etc.

In order to sanely write C code the bear minimum you need is to know and feel
comfortable [writing](http://mrbook.org/blog/tutorials/make/) and
[using](http://www.cs.colby.edu/maxwell/courses/tutorials/maketutor/)
*makefiles*, so the compilation process can be described like in a recipe and
triggered by issuing the `$ make` command.

![](/img/clang/shot3.png){.img-responsive}

Using [make](https://www.gnu.org/software/make/) alone by writing *makefiles*
will take you pretty far, but for larger software you might want to automate
even further all the software ecosystem, so your code can examine the target
system for both static and dynamic libraries, binaries available and configure
it self to adapt to the system and be as portable as possible.
[Autotools](https://www.gnu.org/software/automake/manual/html_node/Autotools-Introduction.html)
to the rescue.

[Learning](https://autotools.io/index.html) and
[using](https://www.sourceware.org/autobook/autobook/autobook.html#Top)
*Autotools* is not much of a trivial task, but when the complexity in your code
starts to get out of hand, taking the effort to use them is worth it!

If your code is needs not only to be Posix systems portable, but also get
compiled on Windows machines, [CMake](https://cmake.org/) rocks!

![](/img/clang/shot4.jpg){.img-responsive}


# The standard C library is your friend

You can't get any better at writing C code if you're not familiar enough with
the [Standard C library
(libc)](https://www.gnu.org/software/libc/manual/html_node/index.html), in
particular I know a lot of people that don't even know the libc mechanism for
error reporting, so be sure you [know
it](https://www.gnu.org/software/libc/manual/html_node/index.html#toc-Error-Reporting-1).


# Use a linter

![](/img/clang/shot5.jpg){.img-left}

A *linter* in case you don't know, is a program that will statically check the
**source code** (not the executable) to find any known non-portable constructs,
vulnerabilities from common programming mistakes and/or bad practices and any
other general coding mistakes that can make your program leak memory, step on
segmentation faults and the like.


[Splint](http://www.splint.org/) is an awesome piece of software that will tell
you a **lot** about what your code might be doing wrong.

You can use it very easily just by specifying the source files like:

    $ splint foo.c bar.c


Most of the *splint* output will be more than suggestions than critical
warnings, but following the *splint* recommendations with poise will make your
code more robust.

You can tune the level of paranoia with the *splint* argument options: `-weak`,
`-standard`, `-cheks` and `-strict`


# Valgrind

![](/img/clang/shot6.png){.img-responsive}

[Valgrind](http://valgrind.org/) is a profiling software with a few neat tricks
up the sleeve. In contrast to *splint*, it will use your **executable program**
and will help you finding memory leaks, make your programs faster and more
correct.

When compiling your program use the `-g` compiler flag so extra debugging
information is include in the executable.

Then you can execute you program with Valgrind like this:

    $ valgrind foobar arg1 arg2

That will use the `Memcheck` tool, one of multiple [Valgrind
tools](http://valgrind.org/docs/manual/manual.html).


# Use a debugger

![](/img/clang/shot7.png){.img-responsive}

Yeah sure, you can fill up you code with `printf` calls for debugging and pretty
much get away with it, but knowing how to use a debugger is always a valuable
skill.

Some debugging sessions will be far more easy with
[GDB](https://www.gnu.org/software/gdb/) than a bunch of `printf` lines all
around, and some times it will not be the case. But for those cases it is,
you'll be a happy programmer.


# Use a control version system

![](/img/clang/shot8.jpg){.img-responsive}

You might think you can get away keeping a ton of directories for each version
of your program if it is small, but that will, eventually, byte you!

A control version system will give you a few super powers for collaboration,
version restoring, multi branching, proper history tracking, back up and so much
more.

You could use [CVS](http://www.nongnu.org/cvs/) or [SVN
(Subversion)](https://subversion.apache.org/), but why to do so if you can use a
much more powerful control version system like
[Mercurial](https://www.mercurial-scm.org/wiki/) or even better
[Git](https://git-scm.com/).

![](/img/clang/shot9.png){.img-responsive}
![](/img/clang/shot10.png){.img-responsive}

On top of that, even if you're working alone in a project and won't collaborate
with more people, using a repository hosting service like
[Bitbucket](https://bitbucket.org/) or [Github](https://github.com/) is a great
way to always have a backup of your code. In the future if more people join to
your project, collaboration will be frictionless.


# Automated documentation

![](/img/clang/shot11.png){.img-responsive}

> Documentation is like sex: when it is good, it is very good; and when it is
> bad, it is better than nothing
> --Dick Brandon

Nobody likes to write and maintain documentation so keep it as automatized as
possible!

Using tools like [Doxygen](http://www.stack.nl/~dimitri/doxygen/) will provide
you with some amazing tricks: documentation generation from source code, multi
target format documentation (HTML, LATEX, PDF, TROFF Man pages, PostScript,
etc).

Remember tu use your abilities writing *Make* recipes to automate the
documentation process as well!

Always write documentation in ways that every possible aspect of it can be
automatized. Don't write documentation using MS Word!! (god dammit!). Use
[Markdown](https://daringfireball.net/projects/markdown/syntax),
[AsciiDoc](http://www.methods.co.nz/asciidoc/),
[DocBook](http://www.docbook.org/).

If you really want a WYSIWYG tool, [Libre Office](https://www.libreoffice.org/)
has a CLI interface that allows you to generate PDF files, so you can add in
your *Make* recipe something like:

    document.pdf: document.odt
        libreoffice --convert-to pdf $<

You can even automatize some graphs generation using
[DOT](http://www.graphviz.org/doc/info/lang.html).


# Unit testing

![](/img/clang/shot12.jpg){.img-responsive}

In a [nutshell](https://en.wikipedia.org/wiki/Unit_testing) *unit testing* is
writing pieces of code that will use the functions of your software and compare
the results to what it is expected to produce. Think of it as writing a program
tu use your program and automatically check if it does what it's supposed to do.

You can take this approach further by doing [Test Driven Development
(TDD)](https://en.wikipedia.org/wiki/Test-driven_development).

Automated tests is fundamental, if you want to write C code in 2016+, start
writing proper test right know! The world will end if you don't.

You could write testing functions for your code by hand or use one of the great
testing frameworks there are for C out there.

I like [Check](https://libcheck.github.io/check/) in particular, it seems to be
the more active one and uses the `make` `check` command so doing `$ make check`
will test your software.

Writing tests with **Check** is easy as pie:

```C
#include <check.h>
#include "../src/foo.h"  // Contains 'multply' function

START_TEST (my_test)
{
    int result = multply(2, 2);
    ck_assert_int_eq(result, 4);
}
END_TEST
```

It should be pretty obvious: the testing function will use the `multply`
function (our tested code) declared in `src/foo.h` and **assert** that the
result of multiplying `2` times `2` is equals to `4`, so next time changes are
made in the `multply` function that makes it misbehave, the bug will be catch
pretty fast and easily when we execute our tests. The example here is a bit
dumb but you get the idea, check every possible edge case. The more robust
the tests are, the more robust the end code will be.


# Learn functional programming

![](/img/clang/shot13.jpg){.img-responsive}

Learning to think functionally will improve your C code despite C being an
imperative language, you'll stop using mutable global state, and all the kind of
stuff that prevent your software from being multi thread safe.

If you work on embedded software, you're probably writing in C. Considering that
even relatively cheap embedded hardware today have more than one core,
parallelism is pretty important and functional programming mind set will help a
lot to do it well.

![](/img/clang/shot14.png){.img-left}\
\

There are quite a few multi paradigm languages out there, like python, but if
I have to give a recommendation I would say: Learn a pure
functional programming language. Specially, [blow your mind with
Haskell!](https://www.haskell.org/)


# Write in C

Eric Raymond [said](http://www.catb.org/esr/faqs/hacker-howto.html):

> The more you can avoid programming in C the more productive you will be.

And a lot of people say similar things, but I **disagree**. C is a great and
powerful language, but with a great power comes a great responsibility. You
don't need to **avoid it**, instead use C when you need and can take advantage
of its power and you can afford the effort it takes handling all the extra
responsibility that power comes with.

![](/img/clang/shot15.png){.img-left}

Depending on what you're doing, some other languages would probably fit better
and give you extra abstraction in exchange of some perforce decrement. In most
cases when you think you need C you probably can also do it well with
[Rust](https://www.rust-lang.org/) or [Go](https://golang.org/) (I recommend the
former) and get the work done with great performance and low level management
when needed.

C is not a monster you have to hide from, it's just a (wonderful) tool. You have
to pick the appropriate tool depending on what you're doing.
