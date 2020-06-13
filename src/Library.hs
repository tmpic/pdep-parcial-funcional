module Library where
import PdePreludat

data Dragon = UnDragon {resistenciaMaxima :: Number, resistenciaActual :: Number, clase :: Clase, habilidades :: [Habilidad], puntosDeHabilidad :: Number} deriving (Show,Eq)

data Clase = Piedra | Fogonero | Embestida | Espanto | Rastreadora | Afilada| Marejada| Misterio | Desconocido deriving (Show,Eq)

data Habilidad = UnaHabilidad {nombre :: String, efecto :: Dragon -> Dragon} deriving (Show,Eq)

chimuelo :: Dragon
chimuelo = UnDragon 100 100 Embestida [controlSobreDragones, disparoPlasma] 5

gordontua :: Dragon
gordontua = UnDragon 150 150 Piedra [endurecerPiel, escupirLava] 3

dragonGenerico :: Dragon
dragonGenerico = UnDragon 1700 1700 Espanto [] 50

-- Punto 1)

calcularFerocidad :: Dragon -> Number
calcularFerocidad dragon = resistenciaActual dragon + ((10 *) . puntosDeHabilidad) dragon

clasificarSegunFerocidad :: Dragon -> Number
clasificarSegunFerocidad dragon | ((200>=) . calcularFerocidad $ dragon) = 1
                                | ((500>=) . calcularFerocidad $ dragon) = 2
                                | ((1000>=) . calcularFerocidad $ dragon) = 3
                                | otherwise = calcularNivel (calcularFerocidad $ dragon) 4

calcularNivel :: Number -> Number -> Number
calcularNivel nivel contador | nivel - 999 <= 1001 = contador
                             | otherwise = calcularNivel (nivel - 1000) (contador + 1)

-- Punto 2.a)

conoceHabilidad :: Habilidad -> Dragon -> Bool
conoceHabilidad habilidadDeDragon dragon = elem (nombre habilidadDeDragon) (map nombre (habilidades dragon))

-- Punto 2.b) modelo habilidades

consumirPuntosHabilidad :: Number -> Dragon -> Dragon
consumirPuntosHabilidad cantidad dragon = dragon {puntosDeHabilidad = max 0 (puntosDeHabilidad dragon - cantidad)}

aumentarPuntosHabilidad cantidad dragon = dragon {puntosDeHabilidad = (+ cantidad) . puntosDeHabilidad $ dragon }

disparoPlasma :: Habilidad
disparoPlasma = UnaHabilidad "disparoPlasma" (consumirPuntosHabilidad 3)

controlSobreDragones :: Habilidad
controlSobreDragones = UnaHabilidad "controlSobreDragones" (consumirPuntosHabilidad 2)

endurecerPiel :: Habilidad
endurecerPiel = UnaHabilidad "endurecerPiel" efectoEndurecerPiel
    where efectoEndurecerPiel (UnDragon resistenciaMaxima resistenciaActual Piedra habilidades puntosDeHabilidad) = consumirPuntosHabilidad 1 (UnDragon resistenciaMaxima resistenciaActual Piedra habilidades puntosDeHabilidad)
          efectoEndurecerPiel dragon = consumirPuntosHabilidad 5 dragon

escupirLava ::  Habilidad
escupirLava = UnaHabilidad "escupirLava" efectoEscupirLava
    where efectoEscupirLava dragon = (consumirPuntosHabilidad ((10 -) . clasificarSegunFerocidad $ dragon) dragon)

alientoDeHielo :: Habilidad
alientoDeHielo = UnaHabilidad "alientoDeHielo" efectoAlientoDeHielo
    where efectoAlientoDeHielo dragon = (consumirPuntosHabilidad (max 1 (resistenciaMaxima dragon - resistenciaActual dragon)) dragon)

usarHabilidad :: Habilidad -> Dragon -> Dragon
usarHabilidad habilidad dragon | conoceHabilidad habilidad dragon = efecto habilidad $ dragon
                               | otherwise = dragon
                            
-- Punto 3 modelo Tareas

reducirResistencia :: Number -> Dragon -> Dragon
reducirResistencia cantidad dragon = dragon {resistenciaActual = (cantidad -) . resistenciaActual $ dragon }

restaurarResistencia :: Dragon -> Dragon
restaurarResistencia dragon = dragon {resistenciaActual = resistenciaMaxima dragon}

type Tarea = Dragon -> Dragon

transportarMercaderia :: Number -> Number -> Tarea
transportarMercaderia kilometros peso = reducirResistencia (kilometros * peso)

descansar :: Tarea
descansar = restaurarResistencia . aumentarPuntosHabilidad 5

derribarTorre :: Tarea
derribarTorre dragon | conoceHabilidad disparoPlasma dragon = usarHabilidad disparoPlasma dragon
                     | otherwise = reducirResistencia 50 dragon

apagarIncendio :: Tarea
apagarIncendio dragon | conoceHabilidad alientoDeHielo dragon = usarHabilidad alientoDeHielo dragon
                      | otherwise = reducirResistencia 40 dragon

ahuyentarDragones :: [Dragon] -> Tarea
ahuyentarDragones dragones dragon | conoceHabilidad controlSobreDragones dragon = usarHabilidad controlSobreDragones dragon
                                  | otherwise = reducirResistencia (10 * (cantidadDragonesSuperiores dragon dragones)) dragon

cantidadDragonesSuperiores :: Dragon -> [Dragon] -> Number
cantidadDragonesSuperiores dragon dragones = length (filter (tieneMasFerocidad dragon) dragones)

tieneMasFerocidad :: Dragon -> Dragon -> Bool
tieneMasFerocidad dragonAliado dragonEnemigo = clasificarSegunFerocidad dragonEnemigo > clasificarSegunFerocidad dragonAliado 

cazarRecompensa :: Number -> Number -> [Dragon] -> Tarea
cazarRecompensa recorrido peso dragones dragon = (transportarMercaderia recorrido peso . ahuyentarDragones dragones) dragon

-- Punto 4

listaGenerica :: [Tarea]
listaGenerica = [cazarRecompensa 10 5 [chimuelo, gordontua, chimuelo, gordontua], apagarIncendio, derribarTorre, descansar]

hacerTareas :: [Tarea] -> Dragon -> Dragon
hacerTareas listaDeTareas = foldl1 (.) listaDeTareas

type Criterio = Dragon -> Bool

cumpleCriterios :: [Criterio] -> Dragon -> Bool
cumpleCriterios criterios dragon = all (\criterio -> criterio dragon) criterios

-- embarcarMision :: Mision -> Dragon
embarcarMision mision dragon | cumpleCriterios (criterios mision) dragon = ((aumentarPuntosHabilidad 5 . aprenderHabilidad (habilidadDeRecompensa mision)) . (hacerTareas (tareas mision)) $ dragon) {resistenciaActual = resistenciaMaxima dragon + length (tareas mision)}
                             | otherwise = dragon

aprenderHabilidad :: Habilidad -> Dragon -> Dragon
aprenderHabilidad habilidad dragon = dragon {habilidades = (habilidades dragon) ++ [habilidad]}

esDeClase :: Clase -> Dragon -> Bool
esDeClase clasePedida dragon = clase dragon == clasePedida

gronckle :: Dragon
gronckle = UnDragon 100 100 Piedra [] 0 

data Mision = UnaMision {tareas :: [Tarea], criterios :: [Criterio], habilidadDeRecompensa :: Habilidad}

asaltoDeGronckles :: Mision
asaltoDeGronckles = UnaMision [apagarIncendio, apagarIncendio, cazarRecompensa 100 30 (take 5 (repeat gronckle))] [conoceHabilidad controlSobreDragones, esDeClase Embestida] escupirLava

-- Implementar una mision

zippleBack :: Dragon
zippleBack = UnDragon 200 200 Fogonero [escupirLava, alientoDeHielo] 4

losCabezaDePingo :: Mision
losCabezaDePingo = UnaMision listaGenerica [conoceHabilidad disparoPlasma, conoceHabilidad controlSobreDragones, esDeClase Espanto] endurecerPiel

-- Punto 5)

elMasApto :: [Dragon] -> Mision -> Dragon
elMasApto dragones mision = maximoSegun (clasificarSegunFerocidad) (losMasAptos dragones mision)

losMasAptos :: [Dragon] -> Mision -> [Dragon]
losMasAptos dragones mision = filter (not . estaExhausto) (map (embarcarMision mision) dragones)

estaExhausto :: Dragon -> Bool
estaExhausto = (==0) . resistenciaActual

maximoSegun f = foldl1 (mayorSegun f)

mayorSegun f a b
  | f a > f b = a
  | otherwise = b