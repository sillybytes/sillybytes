---
title: Vim vs AWK
published: 2015-03-06
...

Vim is a text editor and Awk a text processing language, I wouldn't blame you
for believing that trying to compare them is bonkers, but trust me, I have
something to compare here.

# Case study: [Vinfo](https://www.github.com/alx741/vinfo.git)

*Vinfo* is a Vim plugin that allows you to read Info documentation files right
in a vim session by converting Info plain text files into Vim help-files, so you
get nice syntax highlighting and convenient tags for jumping between the file
contents. Let's examine how it does its job.

<!--more-->

This is how the Info text looks like, note that the titles are underlined using
`*` chars:

```markdown
Some context lines here
maybe a nice introduction
or something.

1. Title
********

This section refers to blah blah
and is great because of blah blah...
```


And here is what we need. Titles underlined using `=`:

```markdown
Some context lines here
maybe a nice introduction
or something.

1. Title
========

This section refers to blah blah
and is great because of blah blah...
```

It appears to be an easy task, but its actually not.

So why is this difficult using Awk? Or even other tools like grep, sed? Or a
combination of all these?

It's because of the context! In this same file we have many other lines that
start with `=` and that we don't care about; We are actually looking for the
lines that start with `=` and are under a line that starts with a number
followed by a dot and followed by a word that has a blank line separation and a
paragraph above and below that are not the beginning/end of nodes but some sort
of description text... Now that's a difficult match!.

It is technically possible to accomplish this using Awk, but it requires some
complicated logic for handling the state of the context, and it grows out of
hand really quickly.

Let's see how this is done using Vim script (from Vinfo code):

```vim
g/\v^$\n.+\n\=+\n^$\n/norm! jjvg_r-\<Esc>
```

One line. It looks scary, but it wasn't written from scratch. The commands were
obtained by performing normal live editing on the text.

It could be said that one of the reasons for this kind of task to become too
complex using Awk, is because of its *per-line processing* nature.

> But in Awk you can use the RS variable in order to handle multi line matches

I hear you protest. Yes, you can, but it doesn't mean that it will become much
easier, you still need to assume a constant and formatted stream of records OR
dynamically change the RS variable to adjust it when needed. If you have a
non-homogeneous text with multi-line matches that are very context prone,
complexity will rapidly become a real pain to handle.


## Vim goes further

With Vim, you can open the target text file and just start doing the editing
while Vim itself is recording your moves (`:h q`). Then you can paste the
recorded commands and moves in a vim script and bang! The script for your
complex editing is written.

When writing an Awk script you need multiple trial and fail iterations to test
your regular expressions. With vim, however, you can visually test all your
regular expressions, make quick changes, improvements and then just paste them
in a vim script when they happen to work.


### Vim regular expressions engine

You can use a regular, convoluted, expression programmatically in a language
like Perl for complex tasks, or you can use Vim's regular expressions engine to
collapse a lot of logic into one single regex.

Let's say you want to process some text in the middle of a line without touching
anything else in that same line, you can do this in Vim by using the `\%V` atom
in order to work with a visual selected area.

> But, obviously, Vim is going to be slower than an Awk script

Actually, it can be, but many milliseconds processing a large file is something
I would prefer over a couple of hours (or days!) of Awk scripting.

> But Vim cannot do multi-threading, so when processing large files it hangs
> until it's done, and I have no option but to wait for it before I can continue
> working!

That's correct, But this will only be a problem if the text processing is meant
to be executed while working in a vim session. When you execute an Awk script in
your shell you must wait for it or open a new terminal session in order to
continue working. The exact same thing happens here. If you wrote a Vim script
for a specific text processing task, you should execute it and wait for it or
get another terminal while the processing takes place (or send it to the
background of course).

> But I'm not a Vim user and don't know how to use it

Perfect!, now you have a good reason for learning Vim, even if you're not
planning to use it as your main text editor.

> So should I completely forget about Awk?

Of course not!, Awk is an awesome tool that allows you to do awesome things, you
should learn it if you don't know it already!. I still use *grep*, *sed*, *tr*,
etc, to perform basic tasks. Awk will always be there to save the day. You can
use it for a quick *one liner* or write down a proper script if needed too.
Being able to use it in combination with other tools is great, which leads us
to...

> Vim cannot be used in combination with other tools using pipes

But it can!, you just need to use the `-` (dash) option, consider the following:

    cat file | grep -i hello | vim -c scriptCommand -

In this contrived example, `cat` pipes a file into grep, that pipes a result
into Vim, which in turn executes "scriptCommand" and perform the text
processing. So really you can use all the tools with normal piping, including
Vim.

    echo text.txt | tr ... | sed ... | awk { print $2 } | vim -c Script -
