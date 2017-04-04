---
title: Ratpoison, Music Control
published: 2015-10-23
...

![](/img/ratmusic/thumbnail.png){#thumbnail}\

This is the first of a series of posts about the [Ratpoison window
manager](http://www.nongnu.org/ratpoison/).

Ratpoison brings great flexibility and allows us to do pretty amazing things
binding custom keys and running external programs and scripts; I'm going to talk
about some of the ones i use in the next posts. For the first one, lets talk
about music.

Some people use a graphical music player like *Amarok* or just play some
*Youtube* videos, others prefer text players like *MOC* or *CMUS*, but we can
reach the top of flexibility using a music daemon like *MPD* and use our
preferred client (*MPC*, *Ncmpcpp*, *Vimpc*). All this options have something in
common: you usually interact with an interface you have to switch to,
interrupting your work flow in order to control your music, but we can do
better.

<!--more-->

Using the Ratpoison key maps functionality we can define a sub mapping bounded
to a master key, So using the key `m` we can define a whole keymap for music
control like this:

```
newkmap music
definekey music space exec ~/music_control.sh toggle
definekey music l exec ~/music_control.sh select_song
definekey music L exec ~/music_control.sh select_playlist
definekey music s exec ~/music_control.sh stop
definekey music i exec ~/music_control.sh information
definekey music n exec ~/music_control.sh next
definekey music p exec ~/music_control.sh previous
definekey music period exec ~/music_control.sh seek+
definekey music comma exec ~/music_control.sh seek-
definekey music greater exec ~/music_control.sh seek++
definekey music less exec ~/music_control.sh seek--
definekey music slash exec ~/music_control.sh search
definekey music r exec ~/music_control.sh playback repeat
definekey music z exec ~/music_control.sh playback random
definekey music y exec ~/music_control.sh playback single
definekey music c exec ~/music_control.sh playback consume
definekey music question help music
bind m readkey music
```


This snippet appears in my `.ratpoisonrc` file, that you can read entirely
[here](https://github.com/alx741/dotfiles/blob/master/ratpoison/.ratpoisonrc).


Here we define a keymap `music` and, at the end, bind it to `m` key so we can
get to it by triggering:

    C-t m

Note that `C-t` is the default Ratpoison escape sequence.

And then pressing the respective music command:

```
space   Pause/Play
l       Select song from playlist
L       Select playlist
s       Stop
i       Playing information
n       Next
p       Previous
.       seek forward
,       seek backward
>       seek forward more
<       seek backward more
/       Search song (in the entire music repository)
r       Toggle -repeat-
z       Toggle -random-
y       Toggle -single-
c       Toggle -consume-
?       Print binded keys help
```


So, for instance, we could visualize the current playing song, and modes states
by doing:

    C-t m i

![](/img/ratmusic/shot.png){.img-responsive}

Here, the first line represents the playing status:

    [|]  Paused
    [>]  Playing


The second line is the song currently playing/paused. The third line is the song
number in the playlist and the time played/remaining. The forth line Marks with
an `*` the enabled modes.

The script depends on
[ratmen](http://www.update.uu.se/~zrajm/programs/ratmen/?M=D).

And you can get it from my
[Dotfiles](https://github.com/alx741/dotfiles/blob/master/scripts/.scripts/ratpoison/music_control.sh)

To try it out be sure to define your Ratpoison bindings referencing the right
location of the script.
