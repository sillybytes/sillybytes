---
title: Ratpoison, Task Warrior Control
published: 2015-11-06
...

![](/img/rattask/thumbnail.png){#thumbnail}\

This is the fourth post about the [Ratpoison window
manager](http://www.nongnu.org/ratpoison/).

When managing tasks there is no better interface than task warrior CLI, that's
for sure, but sometimes having a few-strokes-interface for listing the current
tasks or adding a new simple one comes in handy.

Lets use Ratpoison goodies for this.
<!--more-->

![](/img/rattask/shot.png){.img-responsive}

We can create a new mapping for the letter `t` and assign some bindings for it
using the following Ratpoison configuration snippet:

```
newkmap task
definekey task l exec task_control.sh list
definekey task p exec task_control.sh list_projects
definekey task a exec task_control.sh add
definekey task question help task
bind t readkey task
```

This snippet appears in my `.ratpoisonrc` file that you can read entirely
[here](https://github.com/alx741/dotfiles/blob/master/ratpoison/.ratpoisonrc).

Now triggering the keymap with `C-t t` and one of the commands:

    l       List task
    p       Lists projects
    a       Add new task
    ?       Print binded keys help


The Add task feature is achieved using a script that prompts for a string that
is passed straight forward to task warrior CLI, so think of it as introducing a
new task with the CLI without invoking task as the command but only its
arguments.

And you can get it from my
[Dotfiles](https://github.com/alx741/dotfiles/blob/master/scripts/.scripts/ratpoison/et_phone_home.sh).

To try it out be sure to define your Ratpoison bindings referencing the right
location of the script.
