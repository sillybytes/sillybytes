---
title: Reparando Arch tras actualización (error while loading shared libraries':' libbz2.so.1.0':' Cannot open shared object file)
published: 2012-05-02
...

![](/img/repairarch/thumbnail.png){#thumbnail}\

Hoy, igual que todos los días luego de arrancar mi sistema, lo primero que hice
fue `pacman -Syu` como cualquier *Archer* haría, y mientras se actualizaba el
sistema me ocupaba de mis asuntos, luego de unos minutos regrese a la terminal y
me percaté de que la actualización había fallado, así que repetí `pacman -Syu`
pero *pacman* no iniciaba y daba un mensaje de error:

> pacman: error while loading  shared libraries: libbz2.so.1.0: cannot open
> shared object file: No such file or directory

<!--more-->

Supuse que si reiniciaba el sistema el problema se solucionaría (grave error)
así que hice:

    reboot

Al iniciarse mi sistema y para mi locura no arranca NADA! Ni X server, ni pacman
, ni muchas otras cosas, y tras unos cuantos minutos de gran dolor de cabeza les
traigo la solución:

El paquete que causa el problema es `bzip2` y como no podemos usar pacman, nos
tocará recuperarlo a mano, afortunadamente pacman mantiene una cache de
paquetes, así que lo tomaremos

Como root hacemos

    cp /var/cache/pacman/pkg/bzip2________ /root

Donde el espacio en blanco es para completar según tengamos en nuestro sistema
en mi caso es: `bzip2-1.0.6-4-i686.pkg.tar.xz`

Luego, de regreso en la carpeta personal de root en `/root` extraeremos el
  paquete que acabamos de copiar con:

    tar -xvf bzip2_________

Lo cual nos creará un directorio "usr"

Por ultimo hacemos:

    cp -rfv ./usr/lib/* /usr/lib/


Problema solucionado!
