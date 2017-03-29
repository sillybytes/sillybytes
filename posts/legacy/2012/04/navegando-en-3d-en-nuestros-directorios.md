---
title: Navegando en 3D en nuestros directorios (película "hackers")
published: 2012-04-07
...

![](/img/dir3d/thumbnail.jpg){#thumbnail}\

Recuerdas la película "Hackers"? Cuando los sistemas de archivos son grandes
torres y los personajes vuelan por ellos?

Pues muchas veces la realidad tiene la capacidad de superar a la ficción y es
por esto que hoy les presento un proyecto de software libre que nos permite
navegar por nuestro sistema de archivos en 3D mientras *volamos* sobre él!

<!--more-->

\
![](/img/dir3d/hackers.png){.img-responsive}


Éste es: *tdfsb*

![](/img/dir3d/software.png){.img-responsive}

Pueden encontrarlo en los repositorios oficiales de la mayoría de las
distribuciones. En Arch, por ejemplo:

    pacman -S tdfsb


Si no lo encuentran en los repositorios de su distro lo puede encontrar aquí:

http://www.determinate.net/webdata/seg/tdfsb.html

Pues bien, la primera ves que lo ejecuten, este generará un archivo de
configuración en su directorio home de nombre `.tdfsb` (sin las comillas por
supuesto).

La configuración por defecto resulta poco amigable, la "navegación" con el mouse
es muy veloz y con las teclas de dirección es muy lenta, así que les dejaré mi
archivo de configuración para que lo reemplacen y puedan partir de una
configuración más pulida.


```
# TDFSB Example Config File

BallDetail         = 20     # detail level of the spheres, must be at least 4
StartDir           = /media/Bodega      # directory to start, absolute path
MaxTexSize         = 256    # maximum texture size, must be 2^n, 0 for maximum
WindowWidth        = 400    # width of the window in pixel
WindowHeight       = 300    # height of the window in pixel
FullscreenWidth    = 640    # width fullscreen
FullscreenHeight   = 480    # height fullscreen
FullscreenDepth    = 0      # depth fullscreen, 0 for same as desktop
GridRed            = 0.2    # R
GridGreen          = 0.2    # G components of the grids color, range 0.0 ... 1.0
GridBlue           = 0.6    # B
ImageBricks        = yes    # images have a volume (toggle later)
ShowDotFiles       = no     # show hidden files (toggle later)
AlphaSort          = yes    # sort the files alphabetically (toggle later)
BGRed              = 0.0    # R
BGGreen            = 0.0    # G components of the background color, range 0.0 ... 1.0
BGBlue             = 0.0    # B
FullScreen         = yes     # start in fullscreen mode (toggle later)
ShowCrossHair      = yes    # show the crosshair (toggle later)
ShowGroundCross    = no     # show the cross on the ground (toggle later)
ClassicNavigation  = yes     # mouse buttons for forward and backward (toggle later)
FlyingMode         = yes     # fly in the viewing direction (toggle later)
MaxFPS             = 25     # throttle fps to leave cpu time for other apps (toggle later), 0 for maximum speed
MoveVelocity       = 3.0    # velocity of movement (F1/F2), range 1 ... 20
LookVelocity       = 0.2    # velocity of rotation (F3/F4), range 0.1 ... 2.0
NameRed            = 0.0    # R
NameGreen          = 1.0    # G components of the filename color, range 0.0 ... 1.0
NameBlue           = 0.0    # B
LiftSteps          = 1      # units to lift up per mousewheel step
CustomExecuteString = cd "%s"; xterm& # custom execute string

# Key bindings

KeyFlying          = " "
KeyHelp            = "h"
KeyJumpHome        = "0"
KeyFullScreen      = "f"
KeyDotFilter       = "."
KeyMouseRelease    = "r"
KeyReload          = "l"
KeyCDup            = "u"
KeyImageBricks     = "b"
KeyGLInfo          = "i"
KeyDisplay         = "d"
KeyCrossHair       = "c"
KeyFPS             = "p"
KeyGrndCross       = "g"
KeyShadeMode       = "m"
KeyFileNames       = "t"
KeyAlphaSort       = "a"
KeyClassicNav      = "o"
KeyForward         = "<"
KeyBackward        = ">"
KeyUp              = "1"
KeyDown            = "3"
KeyLeft            = "q"
KeyRight           = "e"
KeySaveConfig      = "s"
KeyFPSThrottle     = "#"
```
