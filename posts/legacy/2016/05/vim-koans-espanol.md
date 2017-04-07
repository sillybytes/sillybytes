---
title: Vim Kōans [Español]
published: 2016-05-04
...

![](/img/vimkoans/thumbnail.png){#thumbnail}\

Esta es la traducción al español de los [vim
kōans](http://blog.sanctum.geek.nz/vim-koans/) por Tom Ryder.

[**¿Qué es un Kōan?**](https://es.wikipedia.org/wiki/K%C5%8Dan)


# El maestro *Wq* y el desarrollador de Windows

El maestro *Wq* se encontraba ayudado a algunos novatos de Vim. Luego de sus
enseñanzas sobre las virtudes de Vim, pregunto si habían preguntas. Un hombre
joven levantó la mano.

<!--more-->

> Maestro, como puedo filtrar la segunda columna de una tabla de texto plano
> para todas las filas que contienen la cadena 'tcp'?

El maestro *Wq* no dijo nada, se volteó al pizarrón detrás de él, y escribió:

    :%!awk '/tcp/ {print $2}'


Hubo un murmullo aprobador por parte de los estudiantes.

> Pero yo desarrollo en Windows...

Balbuceó el estudiante.

El maestro *Wq* se volteó nuevamente, borró el comando, y escribió:

    :v/tcp/d
    :g/tcp/s/\S\+\s\+\(\S\+\)\s.*/\1/

> Qué!!? Eso es muy complicado para una tarea tan simple!

Replicó el estudiante.

El maestro *Wq* se volteó nuevamente, borró el comando, y escribió:

    Microsoft Excel

En ese momento, el estudiante fue iluminado.



# No hay diferencia

Un dia un monje visitó al maestro *Wq*, y preguntó

> Maestro, ¿Cómo será mi código diferente cuando haya dominado Vim?

El maestro *Wq* respondió

> Antes de Vim: declarar, definir, procesar, imprimir.
> Después de Vim: declarar, definir, procesar, imprimir.



# El maestro *Wq* y el acólito de markdown

Un acólito de markdown vino al maestro *Wq* para demostrar su plugin de Vim.

> Mire, maestro

Dijo el acólito

> Casi he terminado las macros de Vim que permiten traducir de Markdown a HTML.
> Mis funciones se entrelazan, mi parser es muy eficiente y los resultados
> casi perfectos. Puede decirse que he dominado el Vimscript, y mi trabajo
> validará Vim como un editor moderno para los desarrolladores iluminados! He
> hecho bien?

El maestro *Wq* leyó el código del acólito por varios minutos sin decir palabra
alguna. Luego abrió un documento markdown, y escribió:

    :%!markdown


El buffer se llenó de HTML al instante. El acólito se echó a llorar.



# El maestro *Wq* y el maestro Unix

Un viejo maestro Unix vino al maestro *Wq*.

> Estoy contrariado, *Wq*.
> Tu enseñas la filosofía de Vim. Vi es sagrado pero Vim no lo es,
> su código se desparrama, sus opciones llenan la memoria, sus binarios
> son vastos, su comportamiento es inconsistente. Esta no es la filosofía de
> Unix. Me temo que has engañado a tus estudiantes. ¿Qué se puede hacer?

El maestro *Wq* asintió

> Estas en lo correcto.
> Vim está dañado. Vamos a arreglarlo.

El viejo maestro Unix estuvo de acuerdo, abrió una shell y escribió:

    $ vi vim.c

Empezó a modificar el código. El maestro *Wq* observó por un momento y luego
preguntó:

> ¿Qué implementación de *vi* estas usando? Nvi? Vim? Elvis?

El maestro Unix respondió:

> No lo se, no importa.

El maestro *Wq* asintió. El maestro Unix se sentó desconcertado por un momento y
cerro el documento sin guardarlo.



# No la mejor herramienta

Una noche durante una tormenta, la casa del maestro *Wq* colapsó. A la mañana
siguiente empezó a reconstruirla usando sus viejas herramientas. Su aprendiz
acudió a ayudarlo, y construyeron por un momento he hicieron un buen progreso.
Mientras trabajaban, el aprendiz empezó a contar al maestro *Wq* sobre sus últimos
logros.

> Maestro, He desarrollado un grandioso script de Vim que da todo tipo de
> información útil sobre el documento. Cuenta palabras, sentencias, párrafos, he
> incluso dice sobre el tipo de documento es, usando las reglas de resaltado. Lo
> uso en mis pipelines todo el tiempo. Es una cosa fabulosa y estoy muy
> orgulloso. Realmente, Vim es la mejor herramienta.

El maestro *Wq* no respondió. Pensando que había molestado a su maestro, el
aprendiz permaneció en silencio y continuó trabajando.

El aprendiz termino de alinear dos vigas y había posicionado un clavo listo para
clavarlo en la madera, pero notó que el martillo estaba lejos de él.

> Me alcanzaría el martillo, maestro?

El maestro *Wq* le pasó al aprendiz una cierra.

En ese momento, el aprendiz fue iluminado.



# El sueño del maestro Pope

[El maestro Pope](https://github.com/tpopea) soñó una vez que era un usuario de
Emacs. Cuando despertó dijo:

> No se si soy Tim Pope soñado que soy un usuario de Emacs, o un usuario de
> Emacs soñando que soy Tim Pope.



# El mejor editor

[El maestro Neil](http://vimcasts.org/) y [el maestro
Wyatt](http://derekwyatt.org/) gozaban de gran fama por sus instrucciones sobre
la filosofía de Vim, y viajaban alrededor del país enseñando.

El maestro Neil habla calmada y pausadamente, su acento se pasea sobre sus
palabras. Pero el maestro Wyatt es muy entusiasta, empieza y termina, su habla
es rápida y enérgica, y su alma fluye sobre sus enseñanzas.

Un día un estudiante preguntó

> ¿Cuál es la mejor forma de enseñar Vim?

Los maestros Neil y Wyatt respondieron al unisono

> ¿Cuál es el mejor editor: vi o ex?

En ese momento, el estudiante fue iluminado.



# La desesperación del estudiante lento

El maestro *Wq* se encontraba comiendo cuando un estudiante entró en su habitación
y cayó sobre sus rodillas. Lagrimas estaban en sus ojos y se veía profundamente
frustrado. El maestro *Wq* dejó el plato y preguntó

> ¿Qué te perturba tanto, hombre joven?

> Maestro, me doy por vencido. Jamas alcanzaré el dominio de Vim!, Nunca
> aprenderé la filosofía de los grandes patriarcas! Nunca alcanzaré la
> simplicidad brutal, la divina eficiencia del uso de Vim!

> ¿Por qué lo dices?

> Soy tu peor estudiante. Mientras yo lucho escribiendo una simple macro, mis
> compañeros escriben macros recursivas con facilidad. Cuando yo intento
> recordar la expresión regular para caracteres vacíos, mis compañeros escriben
> test de Vimscript de gran complejidad. Soy demasiado lento, y estoy
> avergonzado, me temo y he fracasado.

El maestro *Wq* se puso de pie y dijo

> Ven conmigo a la ventana

El estudiante siguió al maestro *Wq* a la ventana y miró a la casa del vecino del
maestro *Wq*. A través de la ventana ambos podían ver a un hombre joven en traje y
corbata, trabajando sobre un documento.

> ¿Qué vez?

Pregunto el maestro *Wq*. El estudiante observó por un momento.

> Ese hombre está usando Microsoft Excel para generar una hoja de calculo.
> Esta actualizando cada celda a mano. No sabe ni siquiera como usar formulas.
> Escribe letras mayúsculas presionando Caps Lock, y luego presionándolo otra
> vez cuando escribió la letra. Es lento! No comprendo. Como puede estar tan
> contento?

> Viendo a este hombre, como puedes tu no estarlo?

Respondió el maestro *Wq*.

El estudiante fue inmediatamente iluminado. Su nombre era Qa, y se convirtió
luego en uno de los mejores maestros.



# Dominio de Vimscript

Un estudiante pregunto al maestro *Wq*

> ¿Cuándo sabré que he dominado el Vimscrip?

El maestro *Wq* respondió

> Cuando nunca lo uses



# La elegía de Vim

Un hombre joven suplico una audiencia con el maestro *Wq* para leer su ultimo
trabajo, una elegía de las glorias de Vim. Con ojos lagrimosos leyó sus
profundas palabras, vertiendo su alma en su veneración por el editor de texto.

El maestro se sentó y escuchó al poeta por un momento. Luego del décimo verso,
levanto su mano.

> Por favor, no más. Tu poema es terrible

El hombre enfureció.

> Maestro *Wq*, seguramente tu puedes apreciar el poema, tu que conoces la gran
> belleza del editor. Como puedes ser tan terso, tan desdeñoso? Incluso escribí
> este poema en Vim!

El maestro respondió

> Lo escribiste en Vim, pero tu métrica es dispareja, tu rítmica inconsistente,
> tus metáforas mezcladas. Has escrito un poema muy malo usando una herramienta
> muy buena. No eres un poeta, y Vim no te hará uno. Muchos estudiantes no son
> programadores, y Vim no les ayudara con eso tampoco.

El hombre protestó

>Vim es eternamente bello. Merece tener una Elegía.

> Vim no es permanente. Nvi no es permanente. Vi mismo no es permanente,
> solamente la naturaleza de Vi. Emacs tiene naturaleza de Vi, nano tiene
> naturaleza de Vi, incluso bloc de notas tiene naturaleza de Vi. Tu visión
> estrecha que has desarrollado no te permite ajustarte al verdadero valor del
> tema de tu poema. Debes irte. Regresa cuando hayas dominado Emacs.

El poeta se fue, profundamente avergonzado. No regresó nunca más.
