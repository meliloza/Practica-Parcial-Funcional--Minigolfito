module Library where
import PdePreludat

doble :: Number -> Number --no dar bola 
doble numero = numero + numero -- no dar bola !!

--Datos iniciales------------------------------------------------------------------------------------------------- 

data Jugador = UnJugador {
nombre :: String,
padre :: String,
habilidad :: Habilidad
} deriving (Eq, Show)

data Habilidad = Habilidad {
fuerzaJugador :: Number,
precisionJugador :: Number
} deriving (Eq, Show)

-- Jugadores de ejemplo
bart = UnJugador "Bart" "Homero" (Habilidad 25 60)
todd = UnJugador "Todd" "Ned" (Habilidad 15 80)
rafa = UnJugador "Rafa" "Gorgory" (Habilidad 10 1)

data Tiro = UnTiro {
velocidad :: Number,
precision :: Number,
altura :: Number
} deriving (Eq, Show)

type Puntos = Number 

-- Funciones útiles
between n m x = elem x [n .. m]

-------------------------------------------------------------------------------------------------------

--Algunos comentarios para empezar a plantear el punto 1) 
--segun la habilidad q le paso, me devuelve un tiro 
--me dice que hay q modelar los palos, entonces nos da 3 tipos de palos de golf
--tmb me dice la precision recibida, osea se refiere a la precision del JUGADOR no la precision del tiro, OJO con eso 


--UnTiro es el constructor 
--decido modelar a los palos como funciones 

--1)--------------------------------------------------------------------------------------------------

type Palo= Habilidad->Tiro

putter :: Palo 
putter habilidad= UnTiro{velocidad= 10, precision= (precisionJugador habilidad)*2, altura= 0}

--en consola: 
--putter (habilidad bart)

madera :: Palo 
madera habilidad= UnTiro{velocidad=100,precision= precisionJugador habilidad /2, altura=5 }

hierro :: Number -> Palo
hierro n habilidad= UnTiro{velocidad= (fuerzaJugador habilidad)*n ,precision= precisionJugador habilidad/n, altura= max (n-3) 0}   --con notacion infija podria ser: altura= (n-3) ´max´ 0

palos :: [Palo]
palos = [putter,madera] ++ map hierro [1..10]  

--2)-----------------------------------------------------------------------------------------------------

golpe :: Jugador -> Palo -> Tiro
golpe  persona palo = palo (habilidad persona)   --palo es una funcion => podemos aplicarla 
golpe' persona palo = (palo.habilidad) persona   --version "mejor" con composicion

--3)------------------------------------------------------------------------------------------------------

type Obstaculo= Tiro->Tiro

{-Planteo para el tunel: 
Un túnel con rampita sólo es superado si la precisión es mayor a 90 yendo al ras del suelo,
independientemente de la velocidad del tiro. Al salir del túnel la velocidad del tiro se duplica, la
precisión pasa a ser 100 y la altura 0.

Entonces sabemos que la condicion va a  ser: si la precisión es mayor a 90 yendo al ras del suelo,
independientemente de la velocidad del tiro.

Y el efecto producido va a ser: la velocidad del tiro se duplica, la
precisión pasa a ser 100 y la altura 0.
-}

--abstraigo esta funcion obstaculoSuperableSi para no repetir la logica en cada obstaculo que voy creando.
obstaculoSuperableSi:: (Tiro->Bool)->(Tiro->Tiro)->Obstaculo
obstaculoSuperableSi criterio efecto tiro | criterio tiro = efecto tiro 
                                          | otherwise = tiroDetenido

tiroDetenido= UnTiro{velocidad= 0, precision=0, altura=0}

tunelConRampita :: Obstaculo
tunelConRampita tiro = obstaculoSuperableSi superaTunel efectoTunel tiro 

superaTunel :: Tiro->Bool
superaTunel tiro= precision tiro > 90 && altura tiro== 0

efectoTunel :: (Tiro->Tiro)
efectoTunel tiro = UnTiro{velocidad= velocidad tiro*2, precision=100, altura=0} 

--para probar en la consola, tengo q construir unTiro!: 
--tunelConRampita (UnTiro{precision=100,altura=0,velocidad=20})
--devuelve: 
--UnTiro {velocidad = 40, precision = 100, altura = 0}

laguna:: Number->Obstaculo
laguna largo tiro = obstaculoSuperableSi superaLaguna (efectoLaguna largo) tiro 

superaLaguna :: Tiro -> Bool 
superaLaguna tiro = velocidad tiro > 80  && (between 1 5 .altura) tiro 

efectoLaguna :: Number->Tiro->Tiro
efectoLaguna largo tiro= UnTiro{velocidad= velocidad tiro, precision=precision tiro, altura= altura tiro / largo } 

hoyo :: Obstaculo
hoyo tiro= obstaculoSuperableSi superaHoyo efectoHoyo tiro

superaHoyo :: Tiro->Bool
superaHoyo tiro= (between 5 20 .velocidad) tiro && altura tiro == 0 && precision tiro > 95

efectoHoyo:: Tiro->Tiro
--efectoHoyo tiro= tiroDetenido
efectoHoyo _= tiroDetenido -- variable anonima porque no nos interesa que tiro recibe, de todas formas va a hacer un tiroDetenido

--4)----------------------------------------------------------------------------------------------------

palosUtiles :: Jugador-> Obstaculo -> [Palo]
palosUtiles persona obstaculo = filter (leSirveParaSuperar ) palos 

leSirveParaSuperar:: Jugador-> Obstaculo->Palo->Bool
leSirveParaSuperar persona obstaculo palo= 