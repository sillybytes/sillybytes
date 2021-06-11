---
title: Why I dislike IDEs
published: 2016-03-16
...

Let's start by taking off of the discussion some important points:

- Using an IDE and taking advantage of what it has to offer doesn't mean you're
  a bad programmer, and using a good text editor doesn't mean you're a good one.
- I believe that relaying too much upon an IDE can *potentially* make you a bad
  programmer.
- I believe that *inescapably depending* on an IDE means you're probably a bad
  programmer.

<!--more-->

# What's wrong with IDEs?

- Your code doesn't compile? It might be your code is perfectly fine, but the
  IDE is the one messing things up due to misconfiguration or other IDE specific
  problem rather than code or tooling.

- I know best which files should be where, or contain what. Stop making my
  directory tree a mess.

- Quit messing with my CVS without my consent. Even if configurable, the default
  should always be to not touch it.

- Too much clicking on stuff: mouse-driven development.

- There is this tendency of replacing proper documentation with IDE wizards, and
  to replace the need to actually *know* what is going on. A sequence of menus
  and GUI item clicks is *not* valid documentation.

- A waste of pixels. I want the code to be the main thing. No screen real state
  should go to waste with icons to perform actions that *should* be triggered by
  the keyboard in the first place.

- Oftentimes they act as [bad programming languages
  enablers](https://dzone.com/articles/ide-bad-programming-language). If it's
  hard to write software in a language using a text editor, and an IDE becomes
  vital; That tells us more about the language than the editor: needless
  verbosity, cumbersome syntax and import statements that are hard to write by
  hand are examples of issues with the *language*. If your language needs a big,
  heavy, slow and bloated program to be used, then the language is the problem.

- Code is text. Text editing capabilities should be the single most important
  aspect, and yet, IDE editing mechanisms are usually only slightly better that
  those of  word processors. Having to use the mouse for basic tasks is already
  bad enough, but having to use the mouse to edit text? That's just too much.

- Programming is already a mentally exhausting task, there is no need to make it
  a *physically* exhausting task as well. Moving your hands from the keyboard to
  the mouse back and ford thousands of times like you're doing cardio, to reach
  some buttons in a GUI or edit text is no good.

- I need my computing resources for compiling and testing, not for text editing.
  My personal rule of thumb is: if the tool I use for code editing takes more
  than 500 milliseconds to start, it is unacceptable. If it needs more than a
  few MB of memory or if it consumes more than 5% of my CPU time, that's just
  atrocious.

- Many, if not most IDEs have some plugin mechanism, but it's never good enough.
  A text editing tool for programming, should be deeply programmable.

- Most of them spam you by default. Update available!, New button to click!,
  Want me to do *x* thing?. If I need something, I will *ask for it*. Fast,
  simple and automatic is one thing, treating the programmer condescendingly is
  another.

- Some IDEs tend to be greedy in such a way that compiling outside the IDE
  becomes a daunting task. Want to compile from your terminal or an automated
  script? Too bad, this codebase was created with the *XYZ* IDE, so using *it*
  is the only way to make it work. This also applies to the redistribution of
  software as *"IDE projects"*, making it cumbersome if not useless when you're
  not using the exact same IDE or using an IDE at all.

- The programmer should be able to work without and IDE. This doesn't
  necessarily mean that you should not use one, but if you *could not* possibly
  do your job without one, then you have a problem. You're supposed to know more
  than the IDE does.
