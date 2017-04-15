---
title: What's wrong with Java?
published: 2017-04-14
...

![](/img/javawrong/thumbnail.png){#thumbnail}\

I've spitted out quite a bit of rant about Java before in [this
post](http://www.sillybytes.net/2016/03/why-do-i-hate-java.html), in
which I basically list the annoyances that the ecosystem around Java imposes and
how those problems are pretty much a big intersection with the problems of IDEs.

Here I'd like to talk about the problems of Java as a language. Although there
are a few problems with Java as a language I can live with them if I have no
other option but to write Java code; My *hate* to Java doesn't really arise from
Java being a bad *language* but rather from its
[surroundings](http://www.sillybytes.net/2016/03/why-do-i-hate-java.html).

<!--more-->

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
you get a
[NullPointerException](https://docs.oracle.com/javase/7/docs/api/java/lang/NullPointerException.html)
and your program crashes as well. So what gives?

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

![](/img/javawrong/good_bad_ugly.jpg){.img-responsive}

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

![](/img/javawrong/leak.jpg){.img-responsive}

*C++* solves all of those problems beautifully by using [Resource Acquisition Is
Initialization](http://en.cppreference.com/w/cpp/language/raii) or RAII for
short. And by the way, if you hit the same kind of problems you face in *C* with
`mallow()` and `free()` but with *C++*'s `new` and `delete`, then you're doing
it wrong.

By using *C++*'s RAII mechanisms you'll never have to remember to free memory,
close files, sockets, database connections or anything else. Java is supposed to
be a higher level language than *C++* isn't it?


## Exceptions Driven Programming

*C++* and a lot of other imperative and OOP languages suffer form the
*Exceptions* problems as well, but Java manages to screw it up even further.

The heavy use of exceptions forces the programmer to write tons of

```java
try {
    ...
}
catch(someExcetption e) {
    ...
}
catch(someOtherExcetption e) {
    ...
}
catch(yetAnotherExcetption e) {
    ...
}
```

The usual alternative is to just:

```Java
try {
    ...
}
catch(Excetption e) {
    System.out.println("An exception has occurred, sorry ¯\_(ツ)_/¯");
}
```

The result of this is that the code that matters, the actual logic we're
trying to encode in the program gets deeply buried in there, making it hard to
read, hard to understand, hard to maintain, hard to modify and awfully ugly.
Although most languages suffer from a variant of this issue, some other
languages handle it gracefully by encoding the possibility of failure in the
type system.



# Everything is an object

As I have already said in my [previous
post](http://www.sillybytes.net/2016/03/why-do-i-hate-java.html), No, not
everything is a object. OOP has a lot of problems on its own, and it deserves
its own post. Here I'm talking about the way Java forces OOP.

Most OOP languages have this paradigm as a *feature*, but still allow for free
functions, free data and so on. The problem with Java being strictly OOP is that
it forces objects even when they don't fit, even when they adversely affect
compostability, modularity or readability.

> The problem with object-oriented languages is they’ve got all this implicit
> environment that they carry around with them. You wanted a banana but what you
> got was a gorilla holding the banana and the entire jungle. – Joe Armstrong

In most languages you can perform *actions*, but in Java, having objects as the
only mean of abstraction, you must have *"actioners"* to perform any *actions*,
and you must force them into existent to do anything. OOP is usually bad in
general, although useful in certain contexts; Java makes it soul up from
everything that is wrong with OOP.



# Performance

Java is both fast and slow, depending on what language you compare it with. When
you compare it with higher level languages, Java is reasonably faster, but when
you compare it with C or C++, Java is miserably slow and heavy on resources.

Taking into account that Java is more a low level language rather than a higher
level one as we have seen, it should be compared to is closes cousins C and C++,
in which case you inevitably conclude it's just slow, very slow.



# Java sits in a dead spot

As we've seen, Java is mostly a low level programming language that doesn't
really provides the benefits of one, while at the same time it pretends to be a
high level language and fails miserably.

This leads to the current situation:

```
| C | C++ | Rust | Java | Ruby | Python | PHP | Perl | Earlang | OCaml | Haskell

|--- Low Level --| ???  |---                  Hight Level                   ---|
```

## Java is a bad low level language

From the low level languages, Java can perfectly be replaced byp *C++*, *RUST*
and others. Both of these languages provides low level capabilities (like
writing operating systems, real time systems and such), while at the same time
providing better high level traits like *C++*'s RAII or *RUST*'s statically
guaranteed thread safety, both of these languages will avoid Java's stupid
`NullPointerException`.


## Java is a bad high level language

From the high level languages, Java can be replaced by virtually **any** other
language. Almost any other language will provide a nicer syntax, better and more
powerful ways of abstraction, more terseness, better tooling, better everything.

This makes Java completely replaceable by any other language, it serves no
particular purpose and is particularly good at nothing.



# Stupid programmers abstractFactory

Professors in computer science Robert B.K. Dewar and Edmond Schonberg, published
[an
article](http://static1.1.sqspcdn.com/static/f/702523/9242013/1288741087497/200801-Dewar.pdf?token=%2B5Thxkc7TmMcmP0qpas4Xaozf%2Bg%3D)
in the "Journal of Defense Software Engineering" discussing how Java is a bad
programming language for CS education, and how it produces programmers that are
incapable of doing actual problem solving.

Java produces programmers that have no idea about how the computer actually
works, how to face complex problems, and the intrinsic need of an IDE only makes
the problem 10 times worst. These people will be completely incompetent if
their IDEs would be taken away for one second.

Moreover, programmers that are only capable of writing Java are notoriously
ignorant in programming languages theory or even CS in general for that matter,
no computation theory knowledge, no algorithms knowledge, no nothing. The issue
goes much more further into the same direction when you take into account IDE.

As Joel puts it in his article ["The Perils of
JavaSchools"](https://www.joelonsoftware.com/2005/12/29/the-perils-of-javaschools-2/)

> I’ve seen that the 100% Java schools have started churning out quite a few CS
> graduates who are simply not smart enough to work as programmers on anything
> more sophisticated than Yet Another Java Accounting Applications

Ah... Java Accounting Applications... It pretty much sums up the skills of
Java-only programmers.
