---
title: HURD, el kernel original de GNU
published: 2012-07-16
...

![](/img/hurd/thumbnail.png){#thumbnail}\


Estuve pensando en escribir una entrada sobre como probar GNU/Hurd, pero voy a
empezar por hacer una introducción a lo que es el HURD.

## Así que... Qué es HURD?

HURD es el Kernel original del sistema operativo GNU, del proyecto del mismo
nombre fundado por Richard Stallman.

El desarrollo de HURD comenzó en 1990, pero nunca se liberó su versión final,
esperada para el 2002. Por eso, su lugar en el sistema operativo GNU lo ocupó el
Kernel Linux.

<!--more-->

Pero en realidad el desarrollo de HURD nunca se detuvo, como se puede apreciar
en el video:

<div class='video embed-responsive embed-responsive-16by9'>
<iframe src="https://www.youtube.com/embed/1YFUY6g5dJ8" frameborder="0" allowfullscreen></iframe>
</div>


HURD intenta superar los núcleos tipo Unix en cuanto a funcionalidad, seguridad
y estabilidad, aun manteniéndose compatible con ellos.

Esto se logra gracias a que HURD implementa la especificación POSIX (entre
otras), pero eliminando las restricciones arbitrarias a los usuarios.

A diferencia de Linux (kernel monolítico), HURD se ejecuta encima de un
microkernel encargado de facilitarle los servicios más básicos.

Pero a diferencia de este (implementado como un único servidor), HURD consiste
en múltiples servidores ejecutándose simultáneamente. En lugar de un solo
programa enorme que controle desde el reloj hasta el manejo de la red, en HURD
cada una de estas tareas es gestionada por un servidor independiente.

En parte, la lentitud del desarrollo de HURD se debe a que éste modelo de
servidores es muy difícil de depurar.

El Kernel HURD es tan complejo que lleva siendo desarrollado desde antes que el
kernel Linux y hasta la fecha aún no tiene una versión estable, en parte también
por la falta de desarrolladores y contribución al proyecto. Aún así este kernel
es mejor y más avanzado, por lo que creo que deberíamos prestarle más
atención.
