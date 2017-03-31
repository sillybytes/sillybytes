---
title: Programando PIC's en GNU/Linux (hardware y software)
published: 2013-08-10
...

![](/img/pic/thumbnail.png){#thumbnail}\

Mi fuerte es la informática más no la electrónica, pero como se habrá podido
apreciar en muchos de mis posts el objetivo de incursionar en un campo aún algo
oscuro para mi es el de lograr conectar las capacidades de la informática para
reflejar con ello algo en el mundo físico.

Así pues mientras nos metemos de cabeza en la ciencia de la electrónica tarde o
temprano usaremos un micro controlador, el cual ya es un eslabón muy intermedio
entre estas dos ciencias tan intimas.

La razón es que un micro controlador (uC) es un dispositivo que implementa
electrónicamente una maquina de Turing completa, y nos permitirá con sus
prestaciones interactuar con circuitos electrónicos mediante algoritmos y
programas de software, y es justamente aquí donde viene el problema...

<!--more-->

Una vez hayamos escrito el software y haberlo compilado para ejecutarse en la
arquitectura de estos pequeños dispositivos, tenemos que de alguna manera
colocar todos esos 0's y 1's en la memoria no volátil del micro controlador para
que este pueda ejecutarlo.

Para esto usaremos el protocolo ICSP que es especifico de los micro
controladores *PIC* fabricados por la empresa *Microchip*. Este protocolo nos
permitirá comunicarnos de alguna forma desde la computadora con el uC para darle
el código del programa que queremos almacene en su memoria.

Para hacerlo necesitamos algo de hardware para conectar la PC y el uC, y el
software que implemente debidamente el protocolo ICSP y que nos permita llevar a
cabo el cometido.

Así que manos a la obra!

# Hardware

El protocolo ICSP además de ser un protocolo digital por supuesto, se
corresponde con 5 pines del uC que son:

* `Vss` Masa o *GND*
* `Vdd` Alimentación de +5V
* `Vpp` Alimentación de +13.2V, pero que en la mayoría de los casos nos bastara
con +12V y es precisamente lo que haremos. Se usa para indicarle al uC que
empezaremos a comunicarnos con él mediante ICSP
* `Data` Señal de datos de entrada y salida, *TTL*
* `Clock` Señal de reloj, *TTL*


Estos pines los podemos identificar fácilmente en el uC que tengamos buscando su
*pinout*, por ejemplo:

En el PIC18F4550 (Podemos usar cualquier PIC), tenemos su pinout:

![](/img/pic/pinout.png){.img-responsive}


* Pin #1: `Vpp`
* Pin #40: `PGD` (Programming Data)
* Pin #39: `PGC` (programmin Clock)
* Pin #11 y #32: `VDD` (Usamos cualquiera de los 2, en otros PIC's habrá
    solamente uno)
* pin #12 y #31: `VSS`


Ahora que sabemos cuales son los pines correspondientes a ICSP en nuestro PIC,
armaremos el hardware que conectará la PC con el uC (PIC). Tendrá 5 cables
correspondientes con los pines de ICSP (*vpp,vdd,vss,data,clock*)  y que
conectaremos con los pines correspondientes en el uC (PIC) los cuales ya
conocemos.

El hardware hace uso del puerto paralelo de la PC, seguramente esto será un
impedimento para muchos, pero existe una gran posibilidad de que si andas en
estas cosas, tendrás un PC viejito y que tenga puerto paralelo, la ventaja es
que el hardware que construiremos es muy barato y simple, por que si quieres uno
USB existen muchos tutoriales de como montar uno o incluso puedes comprarlo en
cualquier tienda de electrónica (algo caro) y usarlo casi sin mayor problema,
pero nosotros estamos aquí para hacerlo de esta manera.

Este es el puerto paralelo, asegúrate de tener una PC que lo tenga antes de
continuar.

![](/img/pic/shot.jpg){.img-responsive}


Hasta aquí todo perfecto, ahora proporciono el circuito que he obtenido tras
mucha prueba y error, así que ya puedo asegurar que este funciona de maravilla!

![](/img/pic/scheme1.png){.img-responsive}


1. En el diseño los  pines en forma de triangulo de color verde etiquetados como
`PPx` (donde *x* es un numero)  corresponden con los pines del puerto paralelo,
así el pin `PP2` se conecta en el pin *#2* del puerto paralelo, y para saber que
pin es ese en el puerto usamos un pinout:

![](/img/pic/scheme2.png){.img-responsive}


No hace falta que nos preocupemos en los datos que tiene, solamente en los
números para saber donde conectar.


2. En el diseño los pines que terminan en un pequeño circulo y que tienen una
etiqueta son los cables que irán finalmente a los pines ICSP del uC (PIC)
[Vpp,Vdd,Data,Clock] y el pin `Vss` lo conectamos de cualquiera de aquellos
triángulos formados de 3 lineas como aquel que está debajo del pin verde `PP25`.

Listo! Ya lo tenemos, ahora solo resta montar este circuito en un protoboard si
lo preferimos y sabemos que cables van al puerto paralelo y que cables a que
pines del uC (PIC).

Pero en mi caso he preferido hacer un montaje para poder usarlo siempre y con
mayor comodidad (lo recomiendo muchísimo), aquí las instrucciones para hacerlo:

Lo montaremos es una placa pre-perforada

![](/img/pic/shot1.jpg){.img-responsive}


Usaremos también un conector "DB25" (Macho)  para facilitarnos la conexión al
puerto paralelo

![](/img/pic/shot2.jpg){.img-responsive}


También algunos conectores para facilitarnos la conexión de alimentación y con
los pines ICSP, en este caso tomaremos los +12V y +5V de una fuente de
alimentación de PC, así que he tomado el conector de una lectora floppy , pero
podemos usar el otro tipo de conector, tomándolo de un viejo HDD o unidad
óptica, así podremos conectar fácilmente una fuente de alimentación y tomaremos
los +12 del cable de color amarillo y los +5V del cable de color Rojo (GND de
los cables negros).

Además use un conector de 5 pines que saque de una vieja impresora, así
tendremos cables separados para conectarlos a los pines ICSP del uC (PIC).

Aquí lo apreciamos en el circuito terminado.

![](/img/pic/shot3.jpg){.img-responsive}


Con los elementos listados en el diagrama del circuito, los montamos en la placa
como si de un protoboard se tratase, pero haciendo las conexiones con estaño
(soldandolas)

![](/img/pic/shot4.jpg){.img-responsive}


Y obtenemos finalmente el circuito grabador conectado y listo para usarse. Claro
que, en cuanto hayamos visto la parte del software (le fuente del PC debe estar
encendida y el LED iluminado indicando que el hardware está encendido).

![](/img/pic/shot5.jpg){.img-responsive}



# Software

Ahora bien, tenemos el hardware construido y debidamente conectado. Necesitamos
el software (para sistemas *unix, GNU/Linux en nuestro caso) para interactuar
con el uC (PIC) mediante el protocolo ICSP.

Para esto usaremos el software *Odyssey* que podemos descargar de aquí:
[Odyssey](https://github.com/alx741/odyssey)

Lo instalamos:

    $ tar -jxvf odyssey-0.6.tar.bz2 $ cd odyssey-0.6 $ ./configure
    $ make
    $ sudo make install

Una vez hecho esto tendremos *Odyssey* instalado en nuestro sistema, así que
procedemos a la respectiva configuración.

Necesitamos indicarle a *Odyssey* que Drivers usar, que puerto usar, que pines
del puerto usar, he incluso si la lógica de algún pin está invertida, pero que
nadie se preocupe al respecto, solo tendremos que descargar el fichero de
configuración que yo he preparado para trabajar con el hardware y ponerlo en el
lugar correcto.

```
[io]
driver=linuxppdev
port=0

[signaldelay]
default=1000

clkpin=3
rdatapin=10
wdatapin=2
vpppin=-6
pwrpin=4  # No usado en este circuito
```

Lo guardamos en un fichero de nombre `odyssey.conf`, y lo ubicamos
en su lugar con:

    # mv odyssey.conf /usr/local/etc/


Listo! Tenemos el software instalado y configurado, ahora haremos una prueba que
nos ayudará a saber si todo está bien, para esto conectamos todo el hardware
debidamente, a excepción del uC (PIC) en lugar de eso dejaremos sueltos los
cables que van a los pines ICSP del PIC, con ayuda de un multímetro (configurado
como voltímetro) conectaremos (con ayuda de un protoboard de preferencia) el
cable negro del multímetro en el cable del pin `Vss` , y dejaremos libre el
cable rojo del multímetro.

En una terminal ejecutamos:

    $ odyssey test

Y obtendremos una ventana que esperará nuestras instrucciones y nos indicará que
pin esta encendido (1) o apagado (0)

Una vez aquí podremos probar los respectivos pines activándolos con un signo (+)
y desactivándolos con un signo (-)

* Vpp = v
* Data = d
* Clock = c

De esta forma para activar el pin `Vpp` usamos +v  y lo desactivamos usando -v,
luego colocamos el cable rojo del multímetro en el pin correspondiente que
queremos probar, y este deberá marcarnos 0v (o casi 0V) cuando el pin está
apagado y 5V cuando está encendido (excepto para `Vpp` que deberá marcar unos
12V [un poco más o un poco menos])

Luego de hacer varias pruebas y estar seguros de que todo funciona como lo
esperado procedemos a conectar nuestro uC (PIC) en los respectivos pines y ya
podremos empezar a hacer cosas con él. Para esto debemos indicarle a *Odyssey*
el PIC que estamos usando, por ejemplo usaremos nuevamente el *PIC18F4550*:

Para verificar si *Odyssey* se comunica bien con el PIC, y si el PIC que le
indicamos se corresponde con el que hemos colocado:

    $ odyssey PIC18F4550 check

Nótese que las letras contenidas en el nombre del PIC son en mayúsculas

Para borrar el programa del PIC:

    $ odyssey PIC18F4550 erase

Para verificar si el PIC está en blanco (vacío):

    $ odyssey PIC18F4550 blankcheck

Para grabar un programa (programa.hex) [compilado] en el PIC:

    $ odyssey PIC18F4550 write programa.hex

Para leer el programa que contiene el PIC:

    $ odyssey PIC18F4550 read programa.hex

`programa.hex` es el fichero donde se escribirá el programa que leamos del PIC.


Ya con esto estamos en la capacidad de hacer cualquier cosa que deseemos con un
micro controlador PIC, asumiendo que sabemos escribir código en C o Assambly
para su arquitectura, pero supongo que si desean saber como grabarlo es por que
ya tendrán un programa que grabar en él.

En GNU/Linux podemos escribir el código para el uC en lenguaje C y compilarlo
con *SDCC* (Small Device C Compiler) que podemos instalar de los repositorios de
nuestra distribución, el código que compilemos nos generará un fichero
`programa.hex` que será el que grabemos en el uC (PIC)
