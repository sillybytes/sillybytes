---
title: Recuperar Arch luego de reinstalar Windows (recuperando grub)
published: 2012-03-17
...

![](/img/arch/thumbnail.png){#thumbnail}\

Muchas personas tienen en su computadora un "dual boot" con su amada
distribución de GNU/Linux (cual fuese) y una instalación de Windows por
necesidad y fuerza mayor.

Es bien sabido que una instalación de Windows parece tener la tendencia natural
a *podrirse* por si solo y sin intervención externa. Tras una instalación
fresca, Windows será lo único que arranque pues la MBR queda sobre escrita.

No me alarmaba la situación, pues cuando usaba Fedora reinstalaba fácilmente
grub ejecutando `grub-install` desde *anaconda* (Fedoreanos ;) ), pero cuando
recordé que ya no tenia esta facilidad (Arch), puse un grito de angustia en el
cielo. Sin embargo luego de una tarde de intento-error, mucha investigación y
sobre todo mucho aprendizaje logré restaurar grub y tener un final feliz.
Expongo lo que hice en caso de que le sirva a algún otro Archer en apuros.

<!--more-->

* Inicié una live CD (Back Track en mi caso)

* Creé una carpeta para montar mi partición de Arch:

```sh
mkdir /media/arch
```

* Monte mi partición de Arch. En mi caso es *sda2*:

```sh
mount /dev/sda2 /media/arch
```

* Este paso me dio mucho dolor de cabeza. Usaremos el */dev* de la live CD en el
  */dev* de la partición de Arch:


```sh
mount --bind /dev /media/arch/dev
```


* Reinstalaremos Grub usando `grub-install`:

```sh
grub-install --recheck /dev/sda
```

La clave esta en usar la opción `--recheck` que se encargara de re-mapear la
unidad, sin esta opción simplemente nos hubiese mandado a llorar con un bonito
error.

`sda` es mi HDD, puedes revisarlo usando `fdisk -l`, (tenemos acceso a
`/dev/sda` gracias al paso anterior, de lo contrario este no existiría)


Listo!! Reiniciamos y tenemos nuestro guapo Grub tal como lo dejamos.
