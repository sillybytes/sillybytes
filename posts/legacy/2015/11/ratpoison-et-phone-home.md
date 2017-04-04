---
title: Ratpoison, E.T. Phone home
published: 2015-11-04
...

![](/img/ratet/thumbnail.png){#thumbnail}\

This is the third post about the [Ratpoison window
manager](http://www.nongnu.org/ratpoison/).

This one is not a productivity improvement, nor a *do-more-with-less* feature,
but a playful way to achieve a pretty common task: SSH to my home lab.

Remember that scene in
[E.T.](https://en.wikipedia.org/wiki/E.T._the_Extra-Terrestrial) In which he
says: "ET phone home" and points with his finger to the sky? Well...

When I hit `C-t E` this pops up in my screen:

![](/img/ratet/shot1.png){.img-responsive}

<!--more-->
Then a new tmux window is opened with a SSH connection to my home lab.

This is achieved using the *xcowsay* program and its ability to use a custom
image, in this case:

![](/img/ratet/shot2.png){.img-responsive}


Using the following Ratpoison configuration snippet:

    bind E exec et_phone_home.sh


This snippet appears in my `.ratpoisonrc` file that you can read entirely
[here](https://github.com/alx741/dotfiles/blob/master/ratpoison/.ratpoisonrc).

The `et_phone_home.sh` script does the following:

* Show the E.T image with the "ET phone home..." message
* Switch to terminal emulator window
* Create a new tmux window with the SSH connection

The ET image is displayed using *xcowsay*:

    xcowsay --image et.png "ET phone home..." --think


Then switch to the terminal emulator window (urxvt in my case):

    ratpoison -c "select urxvt"


Or using the script I use for this purpose:

[app_select.sh](https://github.com/alx741/dotfiles/blob/master/scripts/.scripts/ratpoison/app_select.sh) terminal

And finally create a new tmux window with the SSH connection:

    tmux new-window -c ~ -n "ssh_connection" ssh $IP


The script depends on *xcowsay*
And you can get it from my
[Dotfiles](https://github.com/alx741/dotfiles/blob/master/scripts/.scripts/ratpoison/et_phone_home.sh).

To try it out be sure to define your Ratpoison bindings referencing the right
location of the script.
