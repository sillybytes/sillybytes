---
title: VIM Jump to next/previous help file tags fast
published: 2015-02-20
...

![](/img/vimhelp/thumbnail.png){#thumbnail}\

Every *Vimmer*, advanced or nodb, cannot live without Vim help mechanism and
help-files, we all know that. But there is a useful functionality that vim does
not offer to us while reading its documentation: Jump between tags!

Vim files are full of Tags that act as hyperlinks between different parts in one
or multiple help-files, but moving trough tags/links should be faster and here
is how:

When the cursor is on a tag one can follow it with `ctrl-]` but we need the
cursor there in the first place. So lets use `]g` and `[g` for jumping to next
and previous tag respectively adding this to your `vimrc`:

<!--more-->

```vim
nmap [g <Plug>HelpTagPrevious
nmap ]g <Plug>HelpTagNext

nnoremap <silent> <Plug>HelpTagPrevious :call <SID>HelpTag(1)<CR>
nnoremap <silent> <Plug>HelpTagNext     :call <SID>HelpTag(0)<CR>

function! s:HelpTag(reverse)
    call search('|\S\+|', a:reverse ? 'bW' : 'W')
endfunction
```


PD: Why `]g` and `[g` ?
   - So we avoid conflict with other mappings (like unimpaired plugin)
   - Keeps relation with tags as `g]` is a built-in Vim command
