---
title: Ratpoison, Fuzzy window selection
published: 2016-07-13
...

A nice feature to have is the ability to jump to an arbitrary window by
performing a quick fuzzy search with just a few characters. We can achieve
this by using Ratpoison's flexibility and the fantastic [FZF
tool](https://github.com/junegunn/fzf).

The
[window_select.sh](https://github.com/alx741/dotfiles/blob/master/scripts/.scripts/ratpoison/window_select.sh)
script will do the trick using **FZF**

<!--more-->

```SH
function fzf_select
{
    pattern=$(ratpoison -c "prompt > ")
    if [[ "$pattern" == "" ]];
    then
        exit 0
    fi

    window_list=$(ratpoison -c "windows %c")
    selected=$(echo "$window_list" | fzf -q "$pattern" -1 -0)

    if [[ "$selected" != "" ]];
    then
        ratpoison -c "select $selected"
    else
        ratpoison -c "echo [!] There is no a matching window for \"$pattern\""
    fi
}


case $1 in
    'ratmen')
        ratmen_select
        ;;
    'fzf')
        fzf_select
        ;;
esac
```

This will use Ratpoison to prompt for a fuzzy string and will take you
immediately to the matched window.

But in order to invoke this, a Ratpoison mapping is required:

    bind w exec window_select.sh fzf
