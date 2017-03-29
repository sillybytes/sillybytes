---
title: Capturando imágenes en Python con OpenCV en Arch Linux
published: 2012-04-07
...

![](/img/pyopencv/thumbnail.png){#thumbnail}\

Recientemente me interese por la capacidad de tomar frames con una webcam desde
Python y en busca de como lograr tomar imágenes encontré OpenCV, una librería de
visión artificial para C/C++  que tiene bindings para Python, así pues es
perfecta para jugar con Python y una webcam.

El único problema es la poca documentación existente de esta librería en cuanto
a su uso con Python, pero con un poco de búsqueda he encontrado pequeños
programas de muchas personas que me podrían ayudar a entender su funcionamiento.

<!--more-->

![](/img/pyopencv/python.png){.img-responsive}


El sitio en el que más me interesé fue un ya abandonado blog "python-r2" con
aportes de muchas personas y muchos programillas en Python con OpenCV.

Pues bien, al momento de intentar correr uno de ellos en mi sistema tenia
errores de todos lo sabores y me resultó imposible. Hipersayan (a fellow hacker)
me ayudó con el perfecto lío que me traía y explico un poco de ello:

Instalación de OpenCV en Arch:

    pacman -S opencv

Aquí mi primer problema: en Fedora solía instalar OpenCV y opencv-python, pero
desconocía que en Arch el paquete OpenCV ya lo incluía todo, de forma que ya
podía usar OpenCV con Python... Bueno... Casi.

Como sabemos, los Archers somos "actualizados" por excelencia gracias a nuestro
compañero *pacman*, así pues la versión de Python por defecto es la 3.x y
resulta ser que OpenCV aun no cuenta con la compatibilidad para Python 3.x, solo
Python 2.x  (gracias Hipersayan).

Como solución, podemos ejecutar Python como `python2` para usar nuestra
versión de Python 2.x, o también podemos agregar un alias a nuestro fichero
`.bashrc`:

    alias python='python2'

De esta forma al ejecutar `python` estaremos usando nuestra versión 2.x y NO la 3.x

Con esto ya tenia OpenCV funcionando perfecto en Python, pero al intentar
ejecutar unos de los ejemplos que encontré en *python-r2* (que por cierto
muchas gracias a Arturo, el moderador, por arreglar los links) Para mi
sorpresa estos no arrancaban y me arrojaban muchos pero muchos errores.

Descubrí que en una del las actualizaciones de OpenCV, lo desarrolladores habían
modificado (drásticamente a mi parecer) la manera en que se usa esta librería en
Python, por lo que lo ejemplos quedaron inservibles, pero al mismo tiempo
encontré varios ejemplos de OpenCV con Python en su web oficial, aunque olvidé
donde los encontré ¯\\\_(ツ)\_/¯.  Se los dejo en Mediafire en caso de que les
interese:

[Nuevos ejemplos Opencv en Python](http://www.mediafire.com/?t9c08du00krdfn6)
