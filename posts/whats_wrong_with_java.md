---
title: What's wrong with Java?
published: 2017-04-14
...

There's already a rant about Java in [a previous
post](https://sillybytes.net/2016/03/why-do-i-hate-java.html), in which I
basically list the annoyances that the ecosystem around Java imposes and how
those relate and intersect with the problems of IDEs.

Here I'd like to talk about the issues of Java as a language. Although these are
a significant part of what's wrong with Java, keep in mind that it's only half
of the equation, the other part being its
[surroundings](https://sillybytes.net/2016/03/why-do-i-hate-java.html).

<!--more-->

# Pointers, Pointers everywhere

Java is supposed to be a *pointers-free language*, unlike those *pesky* C and
C++. Pointers, although very powerful, are a low level construct that should not
be present in a higher level language. Most of the time We want to be as far
away as possible from pointers when programming unless lower level memory access
is specifically needed.

The problem is Java *does* have pointers; Moreover, it manages to keep most of
the inconveniences of having pointers while giving none of the benefits of not
having them. If you're not giving me the power of pointers, at least be kind
enough to remove the problems they induce.


## Everything is a reference, everything is a pointer

Java loves to call pointers by the nickname of "references", which is only a way
to pretend that there are no pointers.

Java makes everything a pointer, thus the heavy usage of the `new` keyword as a
way to create a *reference*. Having this references gives the *JVM* the ability
to manage memory with the Garbage Collector of course, but it comes with
negative consequences for the programmer.

## NullPointerException

The book *Elegant Objects* by Yegor Bugayenko says:

> In a nutshell, you're making a big mistake if you use NULL anywhere in your
> code. Anywhere -- I mean it.

And I completely agree with that. The problem here is having to take into
account the possibility of *NULL* in a high level language that supposedly
doesn't have pointers and tries to hide those details from you in the first
place.

In C or C++, when you dereference a *NULL* pointer, you get a *Segmentation
Fault* and your program crashes. In Java, when you try to use a *NULL* reference
you get a
[NullPointerException](https://docs.oracle.com/javase/7/docs/api/java/lang/NullPointerException.html)
and your program crashes as well. So what gives?

You may say that the sources of these crashes are different, the *Segmentation
Fault* comes from the OS trying to stop you from crashing the entire system,
while the *NullPointerException* comes from the JVM that... Well, has nothing
left to do but crash. I don't see how is that any better.

*NullPointerException*s are terribly common in Java, and you have to hunt them
down just as any null pointer dereference bug. And if you're thinking the actual
benefit of this is having the *GC* taking care of the memory instead of having
to remember to manually free memory, then think again, as there are languages
that take care of that without a *GC*, including C++.

Tony Hoare himself calls *NULL* the ["Billion-Dollar
mistake"](https://en.wikipedia.org/wiki/Nullable_type#Compared_with_null_pointers).


## Useless pointers

So java is cluttered with pointers, useless pointers. In *C/C++* pointers are
one of the most powerful constructs, they allow you to get closer to the machine
and control its actions with scalpel precision; In Java you get your programs to
crash due to *NULL* pointers while getting no benefit in exchange.

Pointers in Java percolate up in even more creative ways, take for instance
the Equality comparison problem: When you perform equality comparison `==` what
you're actually comparing is *pointer equality*, not *value equality* for which
you need a special method `equal()`, this is a low level language trait that,
unlike other low level languages, won't put the power on the programmers hand
but just the burden.



# The bad, the worst and the ugly

Java has a lot of additional traits that make it not only a low level language
in disguise, but also in my opinion a bad language in general.

![](/img/javawrong/good_bad_ugly.jpg){.img-responsive}


## Awful Verbosity

Most of the Java ugly verbosity is attributed to its static, strong typing
discipline that forces you to annotate the types of everything, everywhere. But
this is not the type discipline fault.

In Java, you declare, for instance, a vector of integers:

```Java
Vector<Integer> vector = new Vector<Integer>();
```

Or a vector of vectors of integers:

```Java
Vector<Vector<Integer>> vector = new Vector<Vector<Integer>>();
```

And it gets progressively uglier like that. Java's way of dealing with this to
some extent, is the empty *diamond* operator `<>`, so instead we could write:

```Java
Vector<Vector<Integer>> vector = new Vector<>();
```

But that's pretty much as far as it gets. *C++11* on the other hand has the
`auto` keyword to let the compiler do what compilers do best: mechanical,
repetitive, deterministic tasks; Type inference is one of those tasks.

Every time, everywhere a time annotation is needed, you provide one only if it's
necessary to avoid ambiguity, otherwise, just use `auto` and let the compiler do
it for you.


## Resource un-safety

One of the main Java selling points is *Memory Safety*, you see, in C you have
to free your memory with `free()` in the right place, at the right time after
every memory allocation with `malloc()` and friends. If you forget to free your
memory you'll have memory leaks, if you free it twice, or if you free it at the
wrong time you'll have a segmentation fault.

Java on the other hand leverages the Garbage Collector to do it for you, the
problem is, this works for memory only!

Whenever you initialize a socket, or a database connection, or open a file, you
still need to *close* it at the right time; So you still can and will have
resources leakage.

C++ solves all of those problems beautifully by using [Resource Acquisition Is
Initialization](http://en.cppreference.com/w/cpp/language/raii) or RAII for
short. And by the way, if you hit the same kind of problems you face in C with
`malloc()` and `free()` but with *C++*'s `new` and `delete`, then you're doing
it wrong.

By using C++'s RAII mechanisms you'll never have to remember to free memory,
close files, sockets, database connections or anything else. Java is supposed to
be a higher level language than C++.


## Exceptions Driven Programming

C++ and many other imperative and OOP languages suffer form the *Exceptions
driven programming* issue as well, but Java manages to screw it up even further.

The heavy use of exceptions forces the programmer to write tons of:

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

The usual alternative is just:

```Java
try {
    ...
}
catch(Excetption e) {
    System.out.println("An exception has occurred, sorry ¯\_(ツ)_/¯");
}
```

The result of this is that the code that matters, the actual logic we're trying
to encode in the program gets deeply buried, making it hard to read, hard to
understand, hard to maintain, hard to modify and awfully ugly. Although most
languages suffer from a variant of this issue, some other languages handle it
gracefully by encoding the possibility of failure in the type system.

Most programming languages break equational reasoning, but that's pretty common;
Exceptions go further by even breaking the imperative sequentiality (*cough*
GOTO *cough*).



# Everything is an object

As mentioned in a [previous
post](https://sillybytes.net/2016/03/why-do-i-hate-java.html): No, not
everything is an object. OOP has a lot of problems on its own, and it deserves
its own post, but here I'm talking about the way Java enforces OOP.

Most OOP languages have this paradigm as a *feature*, but still allow for free
functions, free data and so on. The problem with Java being strictly OOP is that
it forces objects even when they don't fit, even when they adversely affect
composition, modularity or readability.

> The problem with object-oriented languages is they’ve got all this implicit
> environment that they carry around with them. You wanted a banana but what you
> got was a gorilla holding the banana and the entire jungle. – Joe Armstrong

In most languages you can perform *actions*, but in Java, having objects as the
only mean of abstraction you must have *"actioners"* to perform *actions*, and
must force them into existence to do anything even if it convolutes your code
and logic. OOP is usually bad in general, although useful in certain contexts;
Java makes it so that everything that is wrong with OOP is also the only way.

Those and more are the common pains of *Javaland*, that Steve Yegge describes
wonderfully in [Execution in the Kingdom of
Nouns](http://steve-yegge.blogspot.com/2006/03/execution-in-kingdom-of-nouns.html).


# Performance

Java is both fast and slow, depending on what language you compare it with. When
you compare it with higher level languages, Java is reasonably faster, but when
you compare it with C or C++, Java is miserably slow and heavy on resources.

Taking into account that Java is more of a low level language rather than a high
level one as we have seen, it should be compared to its closes cousins C and
C++, in which case you inevitably conclude that it's just slow, very slow.


# Java sits in a dead spot

As we've seen, Java is mostly a low level programming language that doesn't
really provide the benefits of one, while it pretends to be a high level
language and fails miserably.

This leads to the current situation:

```
| C | C++ | Rust | Java | Ruby | Python | PHP | Perl | Earlang | OCaml | Haskell

|--- Low Level --| ???  |---                  High Level                    ---|
```

## Java is a bad low level language

From the low level languages extreme, Java can perfectly be replaced by C++,
Rust and others. Both of these languages provide low level capabilities, and are
good for systems programming, while providing better high level traits like
C++'s RAII or Rust's statically guaranteed safety. Both of these languages will
avoid Java's `NullPointerException` and resource leaks.


## Java is a bad high level language

From the high level languages extreme, Java can be replaced by pretty much
**any** other language. Almost any of them will provide a nicer syntax, better
and more powerful ways of abstraction, better terseness, better tooling, better
everything.

This makes Java completely replaceable by any other language, it serves no
particular purpose and is particularly good at nothing.


# Bad programmers abstractFactory

Professors in computer science Robert B.K. Dewar and Edmond Schonberg, published
[an
article](http://static1.1.sqspcdn.com/static/f/702523/9242013/1288741087497/200801-Dewar.pdf?token=%2B5Thxkc7TmMcmP0qpas4Xaozf%2Bg%3D)
in the "Journal of Defense Software Engineering" discussing how Java is a bad
programming language for CS education, and how it produces programmers that are
incapable of doing actual problem-solving. Or, as Joel puts it in his article
["The Perils of
JavaSchools"](https://www.joelonsoftware.com/2005/12/29/the-perils-of-javaschools-2/):

> I’ve seen that the 100% Java schools have started churning out quite a few CS
> graduates who are simply not smart enough to work as programmers on anything
> more sophisticated than Yet Another Java Accounting Applications
