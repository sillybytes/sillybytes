---
title: Librerías compartidas (shared libraries), qué son y para qué sirven?
published: 2012-07-17
...

![](/img/sharedlib/thumbnail.png){#thumbnail}\

Una librería es un fichero que contiene código compilado generalmente de varios
ficheros objeto, y que contiene un grupo de funciones para ser usadas en un
programa.

Una librería puede ser de dos tipos:

* Shared Lib (Librería compartida o dinámica)
* Static Lib (Librería estática)


Pero esta vez hablaremos específicamente de las librerías compartidas.

<!--more-->


# Las Librerías compartidas

Las librerías compartidas son aquellas que pueden ser enlazadas o vinculadas a
cualquier programa en tiempo de ejecución, permiten que el código que contiene
pueda ser cargado en memoria una única vez y pueda ser usado por varios
programas.

Por lo que mientras exista código común, usando librerías dinámicas su consigue
que el tamaño de los programas sea menor, además de que la memoria necesaria
también se reduce.

Otra ventaja que ofrecen es que mantiene la modularidad, es decir cualquier
cambio, mejora, o agregado simplemente tiene que ser introducido en el código de
la librería compartida, y solo esta tiene que ser recompilada, sin siquiera
tocar todo el software que utiliza la librería.

Las librerías compartidas pueden conocerse bajo varios nombres:

* El nombre usado por el enlazador ('lib' + el nombre de la librería + '.so')

* Nombre completo ('lib' + el nombre de la librería + '.so' + '.' + numero de
  versión)

* Nombre real ('lib' + el nombre de la librería + '.so' + '.' + numero de
  versión + '.' + numero de subversión + '.' + revisión ), donde la revisión es
  opcional.

Es preciso tomar en cuanta un par de consideraciones en cuanto al
versionamiento:

* El numero de la versión cambia cuando los cambios echos en la librería la
  vuelven incompatible con la versión previa.

* El numero de subversión cambia cuando se realizan cambios (de cualquier tipo)
  en la librería sin que esta pierda compatibilidad.

Este convenio de nombres permite que múltiples versiones de una librería
compartida coexistan en el sistema. De esta forma el enlazador no se preocupa
sobre cual es la ultima versión de la librería instalada en el sistema, una vez
que se instala la ultima versión de una librería automáticamente todo el
software se enlaza con la ultima versión.

Generalmente el nombre usado por el enlazador es el "nombre completo" que en
realidad es un enlace simbólico hacia la librería con el "nombre real".


# Ubicación en el sistema de archivos

Siguiendo los estándares de la jerarquía del sistema de archivos *FHS*:

* Todas las librerías que son cargadas al iniciar el sistema se mantiene en */lib*

* Las librerías que son usadas internamente por el sistema están en */usr/lib*
  Estas no están pensadas para ser usadas directamente por los usuarios o
  scripts de la shell.

* Las librerías que no forman parte de la distribución estándar y que se pueden
  descargar están en */usr/local/lib*.


# ¿Nuevas librerías?

Cuando una librería compartida es creada y ubicada en un directorio estándar es
necesario ejecutar `ldconfig`, el cual se encargará de dejar lista y
accesible nuestra librería.

## ¿Por qué?

Cada vez que ejecutamos un fichero ELF el *loader* se inicia primero para cargar
las librerías que usaremos [el *loader* en si es también un objeto compartido:
/lib/ld-linux.so.x]

El *loader* se encargará de buscar y cargar las librerías compartidas de las
cuales depende nuestro software.

Los directorios en los cuales el *loader* busca las librerías se listan en
*/etc/ld.so.conf*, pero hacer esto le puede tomar mucho tiempo (más de el que
nos gustaría), por lo que `ldconfig` se encarga de crear los links simbólicos
necesarios y además crear una cache en */etc/ld.so.cache*, así que el *loader*
puede usar la información de dicha cache y hacer su trabajo en un tiempo
significativamente menor.

Por esto la importancia de ejecutar `ldconfig` cada vez que se agrega una
librería compartida.

Si tenemos una librería compartida en un directorio no estándar tendremos que
agregar dicho directorio a  */etc/ld.so.conf*  para que pueda ser encontrada y
cargada por el *loader*, y finalmente usada.
