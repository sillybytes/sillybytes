---
title: From PIC to AVR
published: 2016-06-17
...

![](/img/picavr/thumbnail.png){#thumbnail}\

This is my humble contribution to the *PIC vs AVR holy war*.

TL;DR: I was a PIC user but decided I hate it, switched to AVR and love it!

[PIC](https://en.wikipedia.org/wiki/PIC_microcontroller) from
Microchip and [AVR](https://en.wikipedia.org/wiki/Atmel_AVR) from Atmel are both
wonderful microcontrollers for hobbyist and professional as well (I am a
hobbyist only if you're wondering).

I used to love PIC microcontrollers and originally choose them because they are
the most widely available in my location. But there are just so many annoyances
that AVR solves so wonderfully!
<!--more-->

# Hardware

I'm referring to programming hardware here. Then only feasible way I have to
program a PIC uC currently is using my [parallel port PIC
programmer](http://silly-bytes.blogspot.com/2013/08/programando-pics-en-gnulinux-hardware-y.html)
and the completely forgotten, unmaintained *Odyssey* software.

The only feasible way you say? Yes!, getting a PICkit (2,3) in my location
requires a $100 (USD) budget. Any other solution like PICkit clones are not much
cheaper.

I've also tried the [usbpicprog](http://www.usbpicprog.org) but didn't get it to
work. Compiling the host software requires to build not only the CLI (the only I
want) but also the bloated GUI. Burning the bootloader is easy, but burning the
firmware using the bootloader requires me to use *piklab* IDE, and I [hate
IDE's!!](http://silly-bytes.blogspot.com/2016/03/why-do-i-hate-ides.htm)

But the price is the least of the impediments. Using an original Microchip's
PICkit or a clone requires using the *pk2cmd* **privative** software and doing
anything outside MPLAB is a major PITA.

AVR on the other hand allows me to program chips so easily and for so cheap!, A
DAPA or DASA programmer is simple, cheap, fast. A USBTiny or a USBASP is so easy
and so cheap to get online, and every programmer can be easily used with the
completely awesome *avrdude* CLI software.


# Software

## Programming

Yes, Microchip provides a complete, fully compatible IDE (MPLAB) that can run in
Unix\* systems and can use PICkit. [But using an IDE bothers
me](http://silly-bytes.blogspot.com/2016/03/why-do-i-hate-ides.htm), and using
**privative** software that only works with **privative** hardware bothers me
even more!

I want; I demand! A Free Software (Free as in Freedom), Command Line interface
for controlling a Free (Open source is Ok for this) programmer hardware that I
can build my self and doesn't takes a shit load of money from my wallet.

The *Odyssey* Unix\* software that I've mentioned before is a bless!, but
getting (Free) software for a Serial programmer, a PICkit or a PICkit clone is
impossible. Nobody cares about PIC Free tooling, just go and use all the
privative, restrictive stuff that Microchip pushes on you.

*Avrdude* solves just everything! An unified (GPL) tool that can drive any
programmer with any hardware interface. I absolutely love it!


## Compiler

The same problem here, Microchips provide a freeware (**privative**) compiler
--that even restricts some optimization for the freeware user-- and the only
sane way to use it is thought the bloated IDE!

The amazing [SDCC](http://sdcc.sourceforge.net/) solves this... Almost...
Look, I really like SDCC, is an excellent Free Software compiler!  But the PIC
port is not that good, and using it still requires you to use non-free
Microchip's header files and linker mappings.

With AVR, you use *GCC*. YES! The GNU freaking C compiler!! Isn't that
completely awesome!? And you even get a fantastic fully featured GPL
[avr-libc](http://www.nongnu.org/avr-libc/) on top of that!


# Community

I've always struggle looking for PIC information. Sure there is a lot out there,
Microchip's official documentation is very good and professional, but even in
the Microchip's forums you're not able to get the level of community help you
get from AVR's community.

AVR has a hacker/hobbyist/professional Free Software and Open Hardware
centered community that makes it so much better overall.


# Conclusion

All that I've presented can vary enormously depending on what the user
wants/likes to use.

For me, PIC is horrible because I hate IDE's I want to use CLI software only
that I can easily script with, adapt to powerful text editors, run on remote
machines over network and so on. But there is people that hate the command line
interface and can't live without an IDE, so the reasons I don't like PIC and
love AVR might be the reasons is the opposite for you.

Its all about **tooling**. When I say "I don't like PIC" or "I hate PIC",
what I mean is: "I don't like PIC **tooling**". Both PIC and AVR are very
powerful and comparable hardware and I like a lot both *devices*. It's just PIC
tooling is hell, while AVR tooling is heaven... With LED's... And angels... And
beer... Free as in freedom beer...
