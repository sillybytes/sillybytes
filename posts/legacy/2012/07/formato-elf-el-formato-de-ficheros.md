---
title: ELF':' el formato de ficheros ejecutables de Linux
published: 2012-07-17
...

![](/img/elf/thumbnail.png){#thumbnail}\

*ELF* es el estándar de Linux para ficheros de código objeto ejecutables, el
equivalente de un *EXE* en Windows.

ELF soporta:

* Diferentes Procesadores
* Diferentes tipos de codificación de datos
* Diferentes máquinas (sobre las que se ejecuta)

Un archivo que contiene código compilado se conoce como fichero objeto. Hasta
aquí todos lo sabemos, pero quizás no todos saben o están plenamente
consientes de que un fichero objeto puede ser de varios tipos:

<!--more-->

1. Fichero objecto reubicable (relocatable)

Este tipo de ficheros objeto contienen código y datos que pueden ser enlazados
con otros ficheros reubicables para producir un fichero ejecutable o un objeto
compartido (shared object).



2. Fichero objeto compartido (shared object)

Este tipo de objetos son usados por el enlazador (dynamic linker) para
combinarlo con el ejecutable que lo necesita o con otros objetos compartidos. En
*/usr/lib* encontrarán varios objetos compartidos, Estas son las famosas
librerías compartidas o dinámicas (shared library). Así podemos hacer uso de una
función contenida por una de estas librerías, sin necesidad de incluirlo
directamente en nuestro código produciendo un ejecutable de menor tamaño,
además de que distintas piezas de software pueden hacer uso de las librerías
compartidas [quizás dedique una entrada a esto].


3. Fichero ejecutable

Este tipo de ficheros contienen ya el código compilado que luego haciendo uso de
el enlazador se ligará con los demás ficheros objetos de nuestro software
(nuestras librerías, clases, etc) y con los objetos compartidos de los cuales
depende nuestro software.


Luego de la cabecera del formato ELF se ubica una tabla de cabecera del
programa, la cual ayuda a la creación del proceso cuando se ejecuta el programa.

Por esto la tabla de cabecera es obligatoria para ficheros ejecutables
(obviamente), pero es opcional para ficheros objeto o compartidos.

# La cabecera ELF

La cabecera ELF comienza con una estructura llamada "Identificación ELF"


## Identificación ELF

### Identificador [4 bytes]

La "firma" de lso ficheros ELF, ocupa los primeros 4 bytes y contiene:
0x7F + 'ELF'  = 0x7F454C46

### Clase [1 byte]

Para especificar la arquitectura, sea de 32 o de 64 bits
{1=32 bits | 2=64 bits}

### Datos [1 byte]

### Versión [1 byte]

Indica la versión de la cabecera ELF

### Identificador de SO y ABI (Aplication Binary Interface) [1 byte]

Define el SO para el cual fue construido el ejecutable
{0 = UNIX V ABI | 1 = HP-UX | 255 = *Reservado}

### Versión de ABI [1 byte]

Define la versión del ABI (Aplication Binary Interface) para la cual fue
construido el objeto, se usa para conocer si existe incompatibilidad entre
distintas ABI's. Independiente de el anterior.

### Bytes reservados

Marca el comienzo de los bytes reservados de la identificación ELF.

### Tamaño de la identificación ELF

El tamaño total de toda la información hasta este punto.



## Tipo

Identifica el tipo de fichero objeto

{1=Reubicable | 2=Ejecutable | 3=Compartido | 4=Nucleo (core file) }

"nucleo" es más bien una "marca" reservada



## Maquina

Indica la arquitectura especifica requerida para el archivo objeto

    {
    1=AT&T WE 32100
    2=SPARC
    3=Intel 80386
    4=Motorola 68000
    5=Motorola 88000
    6=*Reservado*
    7=Intel 80860
    8=MIPS I Architecture
    9=*Reservado*
    10=MIPS RS3000 Little-endian
    11-14=*Reservado*
    15= Hewlett-Packard PA-RISC
    16=*Reservado*
    17=Fujitsu VPP500
    18=Enhanced instruction set SPARC
    19=Intel 80960
    20=Power PC
    21-35=*Reservado*
    36=NEC V800
    37=Fujitsu FR20
    38=TRW RH-32
    39=Motorola RCE
    40=Advanced RISC Machines ARM
    41=Digital Alpha
    42=Hitachi SH
    43=SPARC Version 9
    44=Siemens Tricore embedded processor
    45=Argonaut RISC Core, Argonaut Technologies Inc.
    46=Hitachi H8/300
    47=Hitachi H8/300H
    48=Hitachi H8S
    49=Hitachi H8/500
    50=Intel MercedTM Processor
    51=Stanford MIPS-X
    52=Motorola Coldfire
    53=Motorola M68HC12
    }

Los demás valores están reservados par ser asignados en el futuro conforme se
necesiten.

## Versión

Define la versión del fichero objeto.

## Entry

Define la dirección virtual a la cual el sistema transferirá el control para
iniciar el proceso.

## Phoff

Contiene el desplazamiento en bytes hasta la tabla de cabecera del archivo
objeto, de no tener una tabla de cabecera esta marca contiene el valor 0.

## Shoff

Contiene el desplazamiento en bytes total de la tabla de cabecera del archivo
objeto a partir de el inicio de la misma, de no poseer tabla de cabecera su
valor es 0.

## Banderas

Contiene banderas del procesador especifico asociado con el fichero.

## Tamaño

Contiene el tamaño total de la cabecera en bytes.

## Phentsize

Contiene el tamaño en bytes de una de las entradas de la tabla de cabecera.
Todas las entradas tiene el mismo tamaño.

## Phnum

Contiene el numero de entradas en la tabla de cabecera. El producto de
(Phentsize * Phnum) da como resultado el tamaño total de la tabla de cabecera.

## Shnum

Contiene el numero de entradas en la tabla de sección de cabecera.

## Shstrndx

Contiene el indice de la tabla de cabecera de cada entrada asociada con la tabla
de nombre de sección.
