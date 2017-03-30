---
title: Reparando y limpiando tablas GPT corruptas (Guardando MBR) [WARNING GPT (GUID Partition Table) detected on]
published: 2013-04-29
...

![](/img/mbr/thumbnail.gif){#thumbnail}\

Hace un par de semanas adquirí un nuevo equipo portátil en el cual he instalado
GNU/Linux (Arch) como SO de cabecera, sin embargo mientras lo hacia he dejado un
par de particiones lógicas no muy grandes destinadas a instalar algo como
FreeBSD, OpenBSD, entre otros.

Pues bien, hoy mientras tenia algo de tiempo y luego de la larga espera para
descargar el DVD de FreeBSD me he dispuesto a instalarlo sobre una de mis
particiones, pero comenzado el trabajo unos cuantos golpes de la tecla Enter de
sobra durante el gestor de particionado del instalador me han dado como
resultado la creación de una tabla GPT indeseada y para colmo mal formada,
y por supuesto con esto no podía leer mis particiones de la tabla MBR para
instalar FreeBSD donde quería hacerlo.

Arranqué mi Arch nuevamente para examinar el problema:

Empecé por revisar como estaba el MBR de mi HDD y *fdisk* se quejaba de la
presencia de una tabla GPT que no podía entender (*fdisk* no sabe leer GPT).

<!--more-->

    $ fdisk -l

```
WARNING: GPT (GUID Partition Table) detected on '/dev/sda'! The util fdisk
doesn't support GPT. Use GNU Parted.

Disk /dev/sda: 500.1 GB, 500107862016 bytes, 976773168 sectors
Units = sectors of 1 * 512 = 512 bytes
Sector size (logical/physical): 512 bytes / 512 bytes
I/O size (minimum/optimal): 512 bytes / 512 bytes
Identificador del disco: 0x7ff32c5b

Disposit. Inicio    Comienzo      Fin      Bloques  Id  Sistema
/dev/sda1   *          63   195318269    97659103+  83  Linux
/dev/sda2       195319808   390635519    97657856    7  HPFS/NTFS/exFAT
/dev/sda3       390636540   392596469      979965   82  Linux swap / Solaris
/dev/sda4       392596470   976773167   292088349    5  Extendida
/dev/sda5       392596533   451185524    29294496   9f  BSD/OS
/dev/sda6       451185588   509774579    29294496   83  Linux
/dev/sda7       509774643   568363634    29294496   83  Linux
/dev/sda8       568363698   976773167   204204735   83  Linux
```

Luego probé con *gparted*, el cual anunciaba también la presencia de una tabla
GPT y me informaba  que haría uso de ella, y así era, obtenía un disco con una
sola partición!


# Respaldo

Antes de meter mano tomaremos precauciones y respaldaremos el MBR de nuestro
HDD:

    # sfdisk -d /dev/sda > recover   ("sda" es nuestro hdd)

Y guardamos el archivo *recover* en una memoria USB o cualquier otro medio
externo (Importante!!)

# Solución

Tan sencillo como eliminar la tabla GPT haciendo uso de *gdisk*

Si no tenemos ya en nuestros sistema instalado *gdisk* lo instalamos con el
gestor de paquetes de nuestra distribución.

Para Arch: `# pacman -S gdisk`

Ahora como root:

    # gdisk

(">>" consola de gdisk)

    >> /dev/sda   (nuestro hdd)

*gdisk* nos preguntará si deseamos examinar el disco con la tabla MBR o GPT,
elegimos MBR

Luego:

    >> x       (para entrar al modo experto)

    >>Expert command: z

* Nos pedirá confirmación para eliminar GPT (respondemos que si)
* Nos preguntará si deseamos eliminar también MBR (respondemos que NO)

Una vez echo esto podemos cerrar *gdisk* y ya tendremos nuestro disco limpio de
toda GPT , y si hemos echo todo bien tendremos intacta nuestra tabla MBR.

Si no lo hemos echo bien estaremos en serios problemas, que se podrán
solucionar si hemos seguido los pasos de respaldo.
