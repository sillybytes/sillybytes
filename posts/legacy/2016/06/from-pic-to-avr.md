---
title: From PIC to AVR
published: 2016-06-17
...

This is my humble contribution to the *PIC vs AVR holy war*.

TL;DR: I was previously a PIC user but decided I hate it, switched to AVR and
love it!

[PIC](https://en.wikipedia.org/wiki/PIC_microcontroller) from Microchip and
[AVR](https://en.wikipedia.org/wiki/Atmel_AVR) from Atmel are both wonderful
families of microcontrollers for the hobbyist and professional as well. I'm
going to argue, however, that AVR is overall better for every purpose and
because of multiple reasons.

<!--more-->

# Hardware

I'm referring to programming/flashing hardware here. The only feasible way I
currently have to program PIC uCs is by using my [parallel port PIC
programmer](https://sillybytes.net/2013/08/programando-pics-en-gnulinux-hardware-y.html)
and the almost forgotten at this point, though amazing *Odyssey* software.

The only feasible way you say? Yes!, at the time of writing, getting a PICkit (2
or 3) requires at least a $100+ (USD) budget. Any other solution like PICkit
clones are not much cheaper either. Not at all a reasonable budget for the 3rd
world hobbyist.

Using a Microchip's PICkit (or a clone) requires using the *pk2cmd*
**privative** software, which means that doing anything outside MPLAB is a major
PITA.

AVR on the other hand, lets you flash chips so easily and for so cheap!, A DAPA
(or DASA) programmer is simple, inexpensive and fast. Both the USBTiny and the
USBASP programmers are readily available at reasonable prices online and can be
used with the *avrdude*  CLI tool, a much welcomed improvement over MPLAB
behemoth.


# Software

## Programming

Yes, Microchip provides a complete, fully compatible IDE (MPLAB) that can run in
Unix\* systems and can talk to PICkit. [But using an IDE pains
me](https://sillybytes.net/2016/03/why-do-i-hate-ides.htm), and using
**privative** software that only works with **privative** hardware pains me even
more.

I want a Free Software (as in Freedom) command line tool to drive a reasonably
priced programmer hardware. The *Odyssey* utility that I've mentioned is a
blessing!, but getting (Free) software for a Serial programmer, a PICkit or a
PICkit clone is impossible, nobody cares about PIC Free tooling, just go and use
all the privative, restrictive stuff that Microchip forces onto you.

*Avrdude* solves everything. A unified (GPL) tool that can drive any programmer
with any hardware interface. I absolutely love it!


## Compiler

The same problem here, Microchip provides a freeware (privative) compiler --that
goes as far as to restrict some optimizations for the freeware user-- and the
only sane way to use it is through the bloated IDE.

The [SDCC](http://sdcc.sourceforge.net/) compiler solves this. Kind of... Look I
really like SDCC, it's an excellent Free Software compiler, but the PIC port is
not that good (yet?), it still requires you to use non-free Microchip's header
files and linker mappings.

With AVR, you get to use the *GCC* port. Yes that's right, the GNU freaking C
compiler! And you also get a fully featured GPL
[avr-libc](http://www.nongnu.org/avr-libc/) on top of that.


# Community

I've always struggled to find help with PIC. Sure there is a lot out there,
Microchip's official documentation is very good and professional, but even in
Microchip's own forums you're not able to get the level of community help you
can get from AVR's community.

AVR has a hacker/hobbyist/professional Free Software and Open Hardware
centered community that makes it so much better overall.


# Conclusion

For me PIC is horrible mostly because I dislike IDE's and prefer to use CLI
tools that I can easily script with, adapt to powerful text editors, run on
remote machines over network and so on. I acknowledge, however, that many
developers feel the opposite way and dislike the command line interface and/or
couldn't live without an IDE, so the reasons I don't like PIC and love AVR might
be the same reasons why you love PIC instead.

It's all about **tooling**. When I say "I don't like PIC", what I really mean
is: "I don't like PIC's **tooling**". Both PIC and AVR have extremely powerful
and comparable hardware. I do like the **devices** from both of them.
