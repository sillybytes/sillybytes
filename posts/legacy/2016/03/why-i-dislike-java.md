---
title: Why I dislike Java
published: 2016-03-28
...

# Everything is an object

No, not everything is an object. Java code is riddled with actions disguised as
objects: *Mutator*, *Traverser*, *Factory*, *Maker*, *Creator*, *Generator*,
*Mediator*. So you don't *mutate*, *traverse*, *make*, *create* or *generate*
anything in a straight forward way, instead you force into existence an object
that can perform the required action. This is one of the primary sources of
clumsiness and messiness in Java, it causes [astonishment and improper behaviors
that you are forced to get used
to](http://www.j-paine.org/objects/objects/objects.html).

A reasonable language has *OOP* as a **feature**, to instead force it everywhere
makes everything unnecessarily convoluted. As Steve Yegge puts it in his
article: [Execution in kingdom of
nouns](http://steve-yegge.blogspot.com/2006/03/execution-in-kingdom-of-nouns.html),
verbs should not be second class citizens.

<!--more-->

# S.O.S

Picture this way to familiar scenario: you're trying to compile your program,
but *javac* is yelling at you with a completely incomprehensible error, so you
head to your search engine of choice hoping to get some answers. And you do:

- Click that button over there
- A pop-up widow will appear
- Fill up those text fields
- Mark that check box
- Click the yellow icon
- Click "OK"
- Finally, restart the IDE

It would seem that almost every possible problem you encounter with Java is
somehow related with the IDE, except you might be using a completely different
IDE, or not be using an IDE at all, and yet somehow, solutions outside of one
seem to be alien in the Java world.

# Use and IDE

James Gosling (aka Java's father) says: [Don't use
Emacs](http://www.computerworld.com.au/article/207799/don_t_use_emacs_says_java_father/).

If you're writing Java, and you're not using an IDE but a good text editor
instead, you're going to have some hard times. This symptom, however tells us
more about the language than it says about de editors.

The language needs, and is dependent upon, a gigantic and bloated program to be
usable. The IDE is the support to hold the crumbling parts, and the afterthought
to try and amend the language shortcomings.

In the sense, it's almost as if the language is not appropriate for direct human
use, instead, it requires an extra layer, an interface (the IDE) between the
human and the language.

<!-- TODO -->
<!-- More on that in: [Why I hate -->
<!-- IDEs](http://www.sillybytes.net/2016/03/why-do-i-hate-ides.html). -->
