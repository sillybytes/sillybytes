---
title: A better and prettier mysql/mariadb CLI client
published: 2016-05-22
...

The default, unconfigured CLI client you use with *MySQl* or *MariaDB* is kind
of awful, no colors, pretty bad query editing capabilities, and the output is
not paginated. Let's improve the experience.


# Client

Start by creating a configuration file `~/.my.cnf` with the following content.

Automatically prompt a password for your user to  avoid the `--user` option
each time

<!--more-->


    [client]

    user = alx
    password


Stop making annoying noises, god dammit!

    no-beep


Is easy to type `^C` by accident, don't kill the session!

    sigint-ignore


If the output is wider than the screen, make it readable

    auto-vertical-output


Because I am a dummy indeed

    i-am-a-dummy


You want completion, don't you?

    auto-rehash



## Prompt & color

    pager = "ccze -A | less -RSFXin"

This will paginate your output using *less* only if the output won't fit in your
screen. Also, colorize it using *ccze* (which can make it a little slower in
some situations, so beware)


    prompt="\n[\d]   (╯°□°）╯- ┻━┻    "

Show me the current database and use the drop table donger as the prompt
(╯°□°）╯- ┻━┻  .... Drop table... Get it?


You might want to configure *ccze* as well with the `~/.ccze` file:

    numbers green
    size yellow


# Input

The CLI client uses
[readline](https://cnswww.cns.cwru.edu/php/chet/readline/rltop.html), so let's
configure it to get vi-like bindings by creating a `~/.inputrc` file with the
content:

    $if mysql
        set editing-mode vi
    $endif

Pretty straight forward, just use vi editing mode for *mysql*.


You can get all the configure files from my
[dotfiles](https://github.com/alx741/dotfiles):

- [.my.cnf](https://github.com/alx741/dotfiles/blob/master/mysql/.my.cnf)
- [.inputrc](https://github.com/alx741/dotfiles/blob/master/mysql/.inputrc)
- [.cczerc](https://github.com/alx741/dotfiles/blob/master/ccze/.cczerc)
