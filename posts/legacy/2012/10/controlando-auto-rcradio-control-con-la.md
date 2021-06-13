---
title: Controlando un auto Radio Control con la PC
published: 2012-10-14
...

Hoy he visto en una repisa el último auto RC de mi infancia que aún quedaba sin
desarmar para convertir en algo diferente. Su destino fue adaptarlo para poder
controlarlo desde la PC usando las teclas de dirección del teclado.

<!--more-->

# Hardware

En la imagen pueden ver al auto y su control, el cual tomaremos y desarmaremos
para sacar por completo su placa de circuito.

Desarmamos y extraemos la circuitería del juguete en cuestión:

![](/img/rccar/shot1.jpg)

![](/img/rccar/shot2.jpg)


Soldamos cables en los puntos de alimentación, y sobre cada uno de los 4
contactos de los actuadores metálicos.

![](/img/rccar/shot3.jpg)
![](/img/rccar/shot4.jpg)
![](/img/rccar/shot5.jpg)


Tomaremos la señal emitida por el puerto paralelo de la PC para saturar un
transistor, el cual "unirá" a través de estos cables los contactos del
interruptor del control, que a su vez accionara el auto.

El esquema de conexión es el siguiente:

![](/img/rccar/scheme1.png)


El montaje en *protoboard* luce así:

![](/img/rccar/shot6.jpg)
![](/img/rccar/shot7.jpg)


La alimentación la he tomado también del puerto paralelo como se puede ver, pero
lo ideal sería usar una batería de 9V separada, para potenciar mejor la placa y
tener un mayor alcance, para hacer esto debemos dejar conectado el cable a masa
(PIN 25 del puerto) y retirar simplemente la conexión del PIN 1 para conectar la
batería (tanto positivo como negativo).

Aquí se ve como conecté en el puerto paralelo (usé el cable de una vieja
impresora para facilitarme el acceso al puerto, pero se puede hacer directamente
en el puerto del mainboard):

![](/img/rccar/shot8.jpg)


# Software

Este auto RC en particular funciona accionando cada par de ruedas laterales de
forma independiente, es decir: para cada movimiento es necesario accionar
simultáneamente 2 interruptores en la combinación adecuada. Para dar marcha
hacia adelante o atrás hay que accionar simultáneamente 2 interruptores del
control, para girar se accionan 2 interruptores opuestos, de forma que un par de
ruedas giren en un sentido y el otro par de ruedas en el sentido opuesto
haciendo así girar el auto.

El software que escribí hace trabajar a el auto de esta forma. Si se quiere usar
un auto cuya dirección es distinta se debe adaptar el código.

Además, el software está escrito para sistemas *Unix, yo específicamente he
usado una distribución GNU/Linux. Recuerda asegurarte de tener instalada la
librería *ncurses* en tu sistema.

[Código en Github](https://github.com/alx741/rc_car_pc_control)


```C
/*
 * ============================================================================
 *
 *       Filename:  rc.cpp
 *
 *    Description:  Código para controlar auto RC por puerto paralelo
 *
 *        Version:  1.0
 *        Created:  12/10/12 20:23:28
 *       Revision:  none
 *       Compiler:  g++
 *
 *         Author:  Daniel Campoverde Carrión [Alx741]
 *
 * ===========================================================================
 */


#include <iostream>
#include <ncurses.h>
#include <sys/io.h>

#define puerto (0x378) //Puede ser (0x278)


using namespace std;

int main(void){

//inicializamos ncurses
initscr();
cbreak();
noecho();
nonl();
intrflush(stdscr, FALSE);
keypad(stdscr, TRUE);


//Abrir puerto y configurar permisos
if(ioperm(puerto,1,1)){
	cout << "Error de permisos" << endl << flush;
    }


    int ch;

    //recibimos las pulsaciones
    //bit1,bit3 -> adelante
    //bit2,bit4 -> atras
    //bit3,bit2 -> derecha
    //bit4,bit1 -> izquierda
    while(true){

		ch=getch();
		if(ch==32) outb(0,puerto);
		if(ch==259) outb(5,puerto); //ascii 259=(arriba)
		if(ch==258) outb(10,puerto); //ascii 258=(abajo)
		if(ch==261) outb(6,puerto); //ascii 261=(derecha)
		if(ch==260) outb(9,puerto); //ascii 260=(izquierda)
		if(ch==27){ outb(0,puerto); break;}
    }


    //finalizamos ncurses (regresamos la consola a la normalidad)
    echo();
    resetty();
    endwin();

    return 0;
}
```

He usado la librería *ncurses* para manejar las pulsaciones de teclado, sin
embargo lo precario del código y la falta de investigación sobre la librería me
ha obligado a usar un mecanismo de *parada*, así pues si pulsamos la tecla
dirección *adelante* el auto avanzará y no se detendrá hasta que pulsemos la
barra espaciadora, y lo mismo sucederá con el resto de teclas. Quizás solucione
este inconveniente usando SDL en otro momento.

Nos ubicamos en el directorio del código y compilamos con (asegúrate
de obtener el *makefile* del repositorio en Github):

    $ make

Ejecutamos como root:

    # ./rc

El programa esperará por nuestras pulsaciones en el teclado y sacara los bits
pertinentes por el puerto paralelo, accionando el auto RC.

Como vemos en el código, lo correcto para finalizar el programa es presionar la
tecla `Esc` para que la terminal vuelva a la normalidad.

Aquí un video de el auto en acción:

<div class='video embed-responsive embed-responsive-16by9'>
<iframe src="https://www.youtube.com/embed/mINnOYImaY8" frameborder="0" allowfullscreen></iframe>
</div>
