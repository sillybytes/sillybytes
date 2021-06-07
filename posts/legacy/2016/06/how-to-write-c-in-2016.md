---
title: How to write C in 2016
published: 2016-06-19
...

Matt wrote a very interesting and totally recommended post about [how to C (as
of 2016)](https://matt.sh/howto-c). Keith Thompson wrote later a very detailed
and rather useful
[critique](https://github.com/Keith-S-Thompson/how-to-c-response/blob/master/README.md)
with some extra notes about Matt's post. I urge you to go ahead and read both
articles.

Here I'd like to point out some things I deem important about tooling.

<!--more-->

![](/img/clang/shot1.jpg)

# Use build tools

This might be obvious to most C programmers, but I've seen quite a lot of
people, novices specially, copying and pasting compilation commands on every
iteration.

Using a build tool will help you automate the building process, but also
testing, distribution package generation, etc.

In order to sanely write C code the bear minimum you need is to know and feel
comfortable [writing](http://mrbook.org/blog/tutorials/make/) and
[using](http://www.cs.colby.edu/maxwell/courses/tutorials/maketutor/)
*makefiles*, so the compilation process can be described as a recipe and
triggered by issuing the `$ make` command.

Using [make](https://www.gnu.org/software/make/) alone by writing *makefiles*
will take you pretty far, but for larger software systems you might want to
automate things even further: examine the target system for both static and
dynamic libraries, binaries available and configure things to adapt to the
system and be as portable as possible.
[Autotools](https://www.gnu.org/software/automake/manual/html_node/Autotools-Introduction.html)
to the rescue.

[Learning](https://autotools.io/index.html) and
[using](https://www.sourceware.org/autobook/autobook/autobook.html#Top)
*Autotools* is not much of a trivial task, but when the complexity in your code
starts to get out of hand, *Autotools* do outweigh the effort of getting a grasp
on it.

If your code needs not only be portable on Posix systems, but also get compiled
on Windows machines, [CMake](https://cmake.org/) is what you need.


# The standard C library is your friend

You can't get any better at writing C code if you're not familiar enough with
the [Standard C library
(libc)](https://www.gnu.org/software/libc/manual/html_node/index.html). I've
seen developers trying to re-invent error reporting for instance, so make sure
to be familiar with [libc's error reporting
mechanisms](https://www.gnu.org/software/libc/manual/html_node/index.html#toc-Error-Reporting-1),
as well as [everything else it has to
offer](https://www.gnu.org/software/libc/manual/html_mono/libc.html), you'll be
pleasantly surprised.

# Use a linter

A *linter*, is a program that will statically check the **source code** (not the
binaries) to find any known non-portable constructs, vulnerabilities from common
programming mistakes, bad practices and any other general coding mistakes that
can cause your program leak memory, step on segmentation faults and the like.

[Splint](http://www.splint.org/) is one such linter. It will tell you a *lot*
about what your code might be doing wrong.

You can use it very easily by specifying the source files like:

    $ splint foo.c bar.c


Most of splint's output will be suggestions rather than critical warnings, but
following its recommendations with poise will make your code more robust.

You can tune the level of paranoia with these arguments: `-weak`, `-standard`,
`-cheks` and `-strict`.


# Valgrind

![](/img/clang/shot6.png)

[Valgrind](http://valgrind.org/) is a *profiling* program with more than a few
neat tricks up its sleeve. In contrast to *splint*, it will use your
**executable program** to help you find memory leaks, make it faster and more
correct.

When compiling your program use the `-g` compiler flag to include extra
debugging information in the executable.

Then you can run you program with Valgrind like this:

    $ valgrind foobar arg1 arg2

That will use the `Memcheck` tool, one of multiple [Valgrind's
tools](http://valgrind.org/docs/manual/manual.html).


# Use a debugger

Yeah sure, you can fill up you code with `printf` calls for debugging and pretty
much get away with it, but you're missing out on the power a proper debugger
brings to the table. Some debugging sessions will be far easier with
[GDB](https://www.gnu.org/software/gdb/) than a bunch of `printf` lines all
around.


# Use a control version system

You might think you can get away with keeping multiple directories for each
version of your program if it's small enough, but that mindset will eventually
bite you. A control version system will give you some superpowers for
collaboration, version restoring, multi-branching, proper history tracking, back
up and so much more.

You could use [CVS](http://www.nongnu.org/cvs/) or [SVN
(Subversion)](https://subversion.apache.org/), but should prefer more modern
systems like [Mercurial](https://www.mercurial-scm.org/wiki/),
[Darcs](https://darcs.net) or [Git](https://git-scm.com/).

Furthermore, even if you're working alone in a project and won't collaborate
with more developers, using a repository hosting service like
[Bitbucket](https://bitbucket.org/), [GitHub](https://github.com/), or
[GitLab](https://gitlab.com/) is a great way to always have a backup of your
code. And in the future, if more people join to your project, collaboration will
be frictionless.


# Automated documentation

> Documentation is like sex: when it is good, it is very good; and when it is
> bad, it is better than nothing
> --Dick Brandon

Nobody likes to write and maintain documentation, so keep it as automated as
possible.

Using tools like [Doxygen](http://www.stack.nl/~dimitri/doxygen/) will provide
documentation generation from source code and multi-target format documentation
(HTML, LATEX, PDF, TROFF Man pages, PostScript, etc).

Remember to use your abilities writing *Make* recipes to automate the
documentation process as well!

Always write documentation in ways that every possible aspect of it can be
automated. Don't write documentation using MS Word!. Use
[Markdown](https://daringfireball.net/projects/markdown/syntax),
[AsciiDoc](http://www.methods.co.nz/asciidoc/),
[DocBook](http://www.docbook.org/).

If you really want a WYSIWYG tool, [LibreOffice](https://www.libreoffice.org/)
has a CLI interface that allows you to generate PDF files, so you can add in
your *Make* recipe something like:

    document.pdf: document.odt
        libreoffice --convert-to pdf $<

You can even automatize some graphics generation using
[DOT](http://www.graphviz.org/doc/info/lang.html).


# Unit testing

![](/img/clang/shot12.jpg)

In a nutshell, [unit testing](https://en.wikipedia.org/wiki/Unit_testing) is
about writing pieces of code that will exercise the functions of your software
and compare the results to what it is expected to produce. Think of it as
writing a program tu use your program and automatically check if it does what
it's supposed to do.

You can take this approach further by practicing [Test Driven Development
(TDD)](https://en.wikipedia.org/wiki/Test-driven_development).


Although you could write test functions by hand, there are some great testing
frameworks that will make things smoother. I like
[Check](https://libcheck.github.io/check/) in particular, running `$ make check`
will test your software.

Writing tests with *Check* is pretty simple, take a look:

```C
#include <check.h>
#include "../src/foo.h"  // Contains the 'add' function

START_TEST (my_test)
{
    int result = add(2, 2);
    ck_assert_int_eq(result, 4);
}
END_TEST
```

This test will use your `add` function, declared in `src/foo.h`, and *assert*
that the result of adding `2` and `2` equals `4`, so next time changes are made
in the `add` function that make it misbehave, you'll catch the bug when running
the tests. Granted this example is over simplistic, but you get the idea. Check
every possible edge case. The more robust the tests are, the more robust your
program will be.


# Learn functional programming

![](/img/clang/shot13.jpg)

Learning how to think functionally will improve your C code despite C being an
imperative language, you'll stop using mutable global state and all the kind of
stuff that prevents your software from being multi thread safe, and correct in
general.

If you work on embedded software, you're probably writing in C. Considering that
even relatively cheap embedded hardware today has more than one core,
parallelism is pretty important and a functional programming mind set will help
you to get it right.

There are many multi-paradigm languages out there, like Python and Ruby for
instance, but my personal recommendation is: Learn a purely functional
programming language, in particular, [blow your mind with
Haskell](https://www.haskell.org/).


# Write in C

Eric Raymond [said](http://www.catb.org/esr/faqs/hacker-howto.html):

> The more you can avoid programming in C the more productive you will be.

And he's got a point. However, I don't believe C is a language you should need
to *avoid*, instead, do write in C when you can take advantage of its power and
can afford the additional effort it takes to handle that power.

Depending on what you're working on, other languages would probably fit better
and give you higher level abstraction with just a small perforce hit. In most
cases, when you think you need C you can probably write it in
[Rust](https://www.rust-lang.org/) or [Go](https://golang.org/) (I recommend the
former) and get the work done with great performance and low level management
only when needed.

C is not a monster you have to hide from, it's just a (wonderful) tool. You have
to pick the right tool for the job. C is the right tool for many jobs.
