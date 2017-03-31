---
title: USB a Serial (UART), FT232 convertido de SSOP a DIP
published: 2014-05-24
...

![](/img/ft232/thumbnail.png){#thumbnail}\

Ahora mismo estoy trabajando en un proyecto en el que necesito comunicación con
el PC, la primera opción sería usar el puerto paralelo de mi vieja y querida
Pentium IV, pero aquello de llevar conmigo esa maquina en su grande y pesado
case... Es algo que ciertamente me desmotiva.

La alternativa sería usar una laptop y comunicar por USB, pero mientras no
termine de implementar el estándar USB (en progreso, espero publicarlo pronto
aquí) no sería posible.

La siguiente solución que se me ocurrió fue usar un FT232, un bonito chip que
implementa un *Comunication Device Class* (CDC) de USB y nos entrega un serial
UART a TTL, es ideal y fabuloso, pero por desgracia viene únicamente en una
presentación poco cómoda SSOP de 28 pines.

Esto fue lo que hice para poder usar este genial chip en una protoboard:

<!--more-->

Diseñe un PCB usando inkscape (poco idóneo y terriblemente incomodo para el
cometido, pero sirvió):

![](/img/ft232/scheme.png){.img-responsive}


De esta forma soldando el diminuto chip en el centro y 28 pinheaders a los
costados podría usarlo como cualquier chip DIP.

Aquí un [PDF](https://drive.google.com/file/d/0B6PhGtPfFvqSTWJMMWNxby1nVHM/edit)
que contiene el diseño repetido varias veces sobre un A4, así puede imprimirse
directamente permitiendo que la escala sea exactamente igual pues dada la
naturaleza del empaquetado del chip, es necesaria una precisión de centésima de
milímetro.

Debo admitir que al principio fui algo escéptico, pues usando el método de
transferencia térmica con una plancha y siendo tan finas las pistas las cosas
jugaban en contra, pero finalmente la transferencia, el atacado del cobre y la
soldadura resultaron sorprendentemente bien he incluso mucho más sencillo de lo
que esperaba.

![](/img/ft232/shot1.jpg){.img-responsive}
![](/img/ft232/shot2.jpg){.img-responsive}
![](/img/ft232/shot3.jpg){.img-responsive}


Ahora que podemos usar el chip como si fuera un DIP podemos implementar uno de
los diseños presentados en su propio datasheet:

![](/img/ft232/scheme1.png){.img-responsive}
![](/img/ft232/scheme2.png){.img-responsive}


Ya podemos usar USB de una forma un tanto burda, solo hasta que este lista esa
ya casi cocinada implementación USB libre.

![](/img/ft232/shot4.jpg){.img-responsive}
![](/img/ft232/shot5.jpg){.img-responsive}
![](/img/ft232/shot6.jpg){.img-responsive}
