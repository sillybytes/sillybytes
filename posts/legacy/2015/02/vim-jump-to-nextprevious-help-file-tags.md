---
title: Quickly jump to Vim's next/previous help file tags
published: 2015-02-20
...

Vim's help system provides convenient tags for jumping between sections, moving
the cursor to the tag itself though is less than convenient.

When the cursor is on a tag, one can follow it with `ctrl-]`, but we need the
cursor to be there in the first place. So let's use `]g` and `[g` to quickly
jump to the next and previous tag respectively by adding this to your `vimrc`:

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
