---
title: What's wrong with Java?
published: 2017-12-31
...

![](/img/javawrong/thumbnail.png){#thumbnail}\

I've spitted out quite a bit of rant about Java before in [this post](), in
which I basically list the annoyances that the ecosystem around Java imposes and
how those problems are pretty much a big intersection with the problems of IDEs.

Here I'd like to talk about the problems of Java as a language. Although there
are a few problems with Java as a language I can live with them if I have no
other option but to write Java code; My *hate* to Java doesn't really arise
from Java being a bad *language* but rather from its [surroundings]().

<!--more-->

# Everything is an object

OOP has a lot of problems on its own, and it deserves its own post. Here I'm
talking about the way Java forces OOP.

... no they're not, as i mentioned in my rant post...

# Stupid programmers abstract factory

In 2008 the U.S. DOD's Center Software Technology Support published in the
"Journal of Defense Software Engineering" an article discussing the
unsuitableness of Java as first learned programming language in education.
Disadvantages given for Java as first language were that students "had no
feeling for the relationship between the source program and what the hardware
would actually do" and the impossibility "to develop a sense of the run-time
cost of what is written because it is extremely hard to know what any method
call will eventually execute".[8] Similarly Joel Spolsky in 2005, criticised
Java as overfocused part of universities' curriculum in his essay The Perils of
JavaSchools.[9] Others, like Ned Batchelder, disagree with Spolsky for
criticizing the parts of the language that he found difficult to understand,
making Spolsky's commentary more of a 'subjective rant'.[10]





# Pointers, Pointers everywhere

Java claims to be a *pointers free language*. Pointers, although elegant and
very powerful, are a low level construct that should not be present in a higher
level language, that's for sure. Most of the time I want to be as far as
possible from pointers when programming unless I really need them, in which case
I will just write in *C*.

The problem is Java does have pointers; Moreover it manages to keep most of the
inconveniences of having pointers while giving almost none of the benefits of
not having them. If you're not giving me the power of pointers, at least be kind
enough to remove the problems they arise!


## Everything is a reference, everything is a pointer

Java loves to call pointers as "references", which is only a way to
pretend that there are no pointers.

Java makes pretty much everything a pointer, thus the heavy usage of the `new`
keyword. Making everything a pointer gives *JVM* the ability to manage memory
with the Garbage Collector of course, but the consequences of this are not as
shallow and beneficial as you'd like to think.

Having no means to manipulate an object other than via *references* weakens data
locality (a problem that other high level languages actually does manage to
solve), and thus cache misses become bread and butter for a Java programmer
which, most likely, isn't aware of it or don't even know what I'm talking about
or how is it important. Which takes us to the problems of Java as a programming
learning language.


## NullPointerException

The book *Elegant Objects* by Yegor Bugayenko says:

> In a nutshell, you're making a big mistake if you use NULL anywhere in your
> code. Anywhere -- I mean it.

And I completely agree with that. The problem is having to take into account the
possibility of *NULL* in a high level language that supposedly doesn't have
pointers.

In *C* or *C++,* when you dereference a *NULL* pointer, you get a *Segmentation
Fault* and your program crashes. In Java, when you try tu use a *NULL* reference
you get a *NullPointerException* and your program crashes as well. So what
gives?

You may say that the sources of this crashes are different, the *Segmentation
Fault* comes from the OS trying to stop you from crashing the entire system,
while the *NullPointerException* comes from the JVM that... Well, has nothing
left to do but crash... I don't see how that is any better.

*NullPointerException* are terribly common in Java, and you have to hunt them
down just as any null pointer dereference bug. And if you're thinking the actual
benefit of this is having the *GC* taking care of the memory instead of having
to remember to manually free memory, then you're wrong, there are ways to
statically take care of that, without having the *GC* behemoth eating your CPU
time every time, but I'll get to that in a moment.


## Useless pointers

So java is cluttered with pointers, useless pointers. In *C/C++* pointers are
one of the most powerful constructs, they allow you to get closer to the machine
and control its actions with scalpel precision; In Java you get your
programs to crash due to *NULL* pointers while getting nothing on exchange.

But pointers in Java percolates in even more creative ways, take for instance
the Equality comparison problem: When you perform equality comparison `==` what
you're actually comparing is *pointers equality*, not *values equality* for
which you need a special method `equal()`, this is a low level language trait as
its best, not to mention terribly counter intuitive.



# The bad, the worst and the ugly

Java have a lot of additional traits that make it not only a low level language
in disguise, but also a bad language in general.

## Portability

People usually get confused by the "Java is portable" thing. When we say that
"Java is portable" what we actually mean is that Java *Bytecode* can be ported.

Lower level languages as *C* or *C++* are portable too, the difference is that
these need to be recompiled for the target system. You don't believe me? Take a
look at [Gimp](https://www.gimp.org/), [GCC](https://gcc.gnu.org/) and
[Inkscape](https://inkscape.org/en/), just to mention a few.

When you distribute *C/C++* software for multiple platforms you distribute the
binaries compiled for each platform, while with Java you just distribute one
*Bytecode* executable for every platform, sure, it is a benefit, but no where
near a big one.


## Awful Verbosity

Most of the Java ugly verbosity is attributed to its static, strong typing
discipline, that force you to annotate the types of everything, everywhere. But
this is not the type discipline fault.

In Java you declare, for instance, a vector of integers:

```Java
Vector<Integer> vector = new Vector<Integer>();
```

Or a vector of vectors of integers (a Matrix of integers):

```Java
Vector<Vector<Integer>> vector = new Vector<Vector<Integer>>();
```

Java's way of dealing with this to some extent, is the *diamond* operator `<>`,
so instead we could write:

```Java
Vector<Vector<Integer>> vector = new Vector<>();
```

But that's pretty much as far as it gets. *C++11* on the other hand has the
`auto` keyword to let the compiler do what compilers are good at: mechanical,
repetitive, deterministic tasks; Type inference is one of those tasks.

Every time, everywhere a time annotation is needed, you provide one only if it's
absolutely needed to avoid ambiguity, otherwise, just use `auto` and let the
compiler do it for you.


## Resources un-safety

One the main Java selling points is *Memory Safety*, you see, in *C* you have to
free your memory with `free()` in the right place, at the right time after every
memory allocation with `malloc()` and friends. If you forget to free your memory
you'll have memory leaks, if you free it twice, or if you free it at the wrong
time you'll have a segmentation fault.

Java on the other hand leverages the Garbage Collector to do it for you, the
problem is, this works for memory only!

Whenever you initialize a socket, or a database connection, or open a file, you
still need to *close* it at the right time; So you still can and will have
resources leakage.

*C++* solves all of those problems beautifully by using [Resource Acquisition Is
Initialization](http://en.cppreference.com/w/cpp/language/raii) or RAII for
short. And by the way, if you hit the same kind of problems you face in *C* with
`mallow()` and `free()` but with *C++*'s `new` and `delete`, then you're doing
it wrong.

By using *C++*'s RAII mechanisms you'll never have to remember to free memory,
close files, sockets, database connections or anything else. Java is supposed to
be a higher level language than *C++* isn't it?




## Stop the lies

The type system sucks

The type system lies to me
    You're telling me that if I pass an Integer it will giveme an Integer back,
    but some times it will just explode in my face with a huge stack trace that
    nobody understands or care about



## Performance
    Java is both fast and slow, depending on which language you compare it with.
    When you compare Java with higher level languages, Java is reasonably
    faster, but when you compare it with C or C++ Java is miserably slow and
    heavy on resources. Taking into account that Java is more a low level
    language rather than a higher level one, it should be compared to is closes
    cousins C and C++, in which case you conclude it is just slow, very slow.

# Java sits in a dead spot

Java can be replaced by low level languages, or high level languages

Java uses most of the syntax and semantics of *C* and *C++* and allows the same
constructs that you can build in *C++*

