---
title: Keyboard pedal for Vim
published: 2012-08-20
...

I've seen a USB Vim pedal before that triggers the `i` key, so we're able to
enter in *Insertion* mode with our foot. I want to build something similar, but
focus on practical utility: trigger the `Esc` key, that is actually further
away. And simplicity: use an old keyboard rather than a custom circuit.

<!--more-->

When we push a key, two contacts close a circuit and the keyboard sends the
appropriate *ScanCode*, which is then converted to a *KeyCode* that the OS can
interpret, so we need to find out which combination of contacts in the circuit
correspond to the required key. We have two options:

- Disassemble the keyboard en follow all the traces from the key to the circuit.

- Use software to tell us what *KeyCode* has been received, so we can use a
  wire to connect every possible combination in the keyboard circuit until we
  find the correct one. I'm using GNU/Linux so the `showkey` command will come
  in handy for this.

Let's go for the second option. As root, run:

    # while [ 1=1 ]; do showkey -k; sleep 1; done

This way `showkey` will show us every key press (press and release).

![](/img/vimpedal/shot4.jpg)

Now we can disassemble the keyboard (with the shell loop running). Take off the
circuitry and use a wire to connect every contact until the appropriate
*keycode* shows up in the screen. In this case, we're looking for a *keycode*
**1**, that corresponds with the `Esc` key. I've found it by connecting pin 3 in
the first row with pin 5 in the second row.

Now we can scratch the pin's surface a little bit to revel the copper, but just
enough so we can solder the wires, a couple millimeters will do.

![](/img/vimpedal/shot5.jpg)

Now we can solder the wires to the pins:

![](/img/vimpedal/shot6.jpg)
![](/img/vimpedal/shot7.jpg)


Now just put the circuit back in place, and use some hot glue so the wires stay
in place.

![](/img/vimpedal/shot8.jpg)


For the peda, glue a switch in a cassette case and solder the wires.

![](/img/vimpedal/shot9.jpg)

Now we can always have our fingers in the home row while editing.
