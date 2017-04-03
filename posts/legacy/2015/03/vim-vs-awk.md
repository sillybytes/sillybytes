---
title: Vim vs AWK
published: 2015-03-06
...

What!? Vim is a text editor and Awk a text processing scripting language, how do
you even compare them!? Yes, I'm aware of it, but trust me, I have something to
compare here, so take a breath and continue reading...

What I'm actually trying to compare here is vim scripting vs awk, still don't
make sense? Well lets take a look of this.


# Study case: [Vinfo](https://www.github.com/alx741/vinfo.git)

*Vinfo* is a Vim plugin that allows you to read Info documentation files right
in a vim session by converting the Info plain text files into Vim help-files so
you get a nice syntax highlighting and very convenient tags for jumping between
the file contents. Lets examine how it does its job.

<!--more-->

This is how the Info text looks like, note that the titles are underlined using
`*` chars:

```markdown
Some context lines here
maybe a nice introduction
or something.

1. Title
********

This section refer to bla bla
and is great because of bla bla...
```


And here is what we need, with titles underlined using `=`:

```markdown
Some context lines here
maybe a nice introduction
or something.

1. Title
========

This section refer to bla bla
and is great because of bla bla...
```


It appears to be a very easy task!, but its actually not.

Now, why is this difficult using Awk? Or even other tools like grep, sed? Or a
combination of all these?

It's because of the context! In the file we have other lines that starts with
`=` that we don't care about, and we are looking for the lines that starts with
`=` that are under a line that starts with a number followed by a dot and
followed by a word, that have a blank line separation and a paragraph above and
below that are not start/end of nodes but some sort of description text. That is
a difficult match!.

It is actually possible to do it using only Awk, but it requires code for handle
the state of context, and so the code rapidly grows and increase in complexity
that, we'll see, can be easily handled using Vim script.

Lets see how this is done using Vim script (from Vinfo code):

```vim
g/\v^$\n.+\n\=+\n^$\n/norm! jjvg_r-\<Esc>
```


On simple line that wasn't even written from scratch, because the normal
commands were obtained by doing a normal live edition on the text.

We could say that one of the reasons for this kind of task to become complex
using Awk is because of its -per line processing- nature.

> But in Awk you can use the RS variable in order to handle multi line matches

Yes you can, but it doesn't mean that things will become much more easier,
you'll need to assume a constant and formatted stream of records OR dynamically
change RS variable to adjust it when need it. If you have a non-homogeneous text
with multi line matches and very context prone, complexity will rapidly becomes
a real pain to handle.


# Vim goes one step further (a really big one).

When it comes to text processing Vim script puts a rocket under your feet!

You can open the target text and just start doing the edition while Vim is
recording your moves (`:h q`), then you just go and paste the recorded movements
in your vim-script and bang! The script code for your edition is done.

When writing a Awk script you need various try-fail cycles testing your regular
expressions and see if you are catching what is supposed and verify if the
editions are successful, nothing uncommon when programming right?

But in vim you can visually test all your regular expressions, make quick
changes, improvements and then just paste them in the vim script when they work.
And that's nothing compared to the ability of making the changes as a normal Vim
editing session and then directly putting that moves in your final script so you
actually don't even need to think "how to manipulate this to become that", so
the process is natural and pretty damn fast.


# Vim regular expressions engine

You can use a regular expression programmatically in a language like Perl for
complex tasks, or you can use Vim's regular expressions engine goodness to
collapse a lot of logic into one single regex.

Lets say you want to process some text in the middle of a line without touching
anything else in the line, you can do this in Vim by using the `\%V` atom in
order to work with a visual selected area.

There are even expression atoms that gives you clarity, take for instance the
`\zs` and `\ze` atoms that you can use instead of `( )` expression groups, it
may appear not a big deal but there are situations where it's so much clearer.
Take a look of `:h pattern`.

> But, obviously, Vim is going to be slower than an Awk script

Actually it can be, but just in a few milliseconds processing a huge file, and
we really should prefer to lose a few milliseconds of processing over a few
extra hours (or even days!) of Awk scripting.

> But Vim cannot handle multi threading so when executing a big text processing
> it hangs until it ends and I have no option but wait for it before continue
> working!

That's very truth!, BUT this will only be a problem if the text processing is
meant to be executed while working in a vim session.

When you execute a Awk script in your shell you must wait for it to end or open
a new terminal session in order to continue working. The exact same thing
happens here, if you wrote a Vim script for a concrete text processing you
should execute it and wait for it or get another terminal while the processing
takes place (or send it to the background of course). In both cases you'll use a
terminal for the -long time- processing and then get another one for continue
working, whether if it implies using Vim or not.

> But I'm not a Vim user and don't know how to use it

Great!, now you have a good reason for learning Vim, even if you will not use it
as your main text editor :)

> So should I completely forget about Awk?

Of course not!, Awk is an awesome tool and allows you to do awesome things, you
should learn it if you don't know it already!. I still use *grep*, *sed*, *tr*,
etc to perform basic tasks, Awk will always be there to save the day. You can
use it for *one line command* processing or write down a proper script if
needed. Being able to use it in conjunction with other tools is great too...
Which leads us to...

> Vim cannot be used in conjunction with other tools, because of its nature

Well.. It actually can!, you just need to use the `-` (dash) option, consider
the following:

    echo file | grep -i hello | vim -c scriptCommand -

In this example `echo` pipes a "file" file into Vim, which in turn execute
"scriptCommand", and do the text processing. So notice that you can use all the
mentioned tools to do big team work! Just by standard piping, because now you
know you can do it with Vim!

    echo text.txt | tr ... | sed ... | awk { print $2 } | vim -c Script -

# Conclusion

You shouldn't take the title so literally and put Vim against Awk because you
can still use all this tools cooperatively and conquer the *text* world :)
