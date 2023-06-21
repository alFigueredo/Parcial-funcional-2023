module Library where
import PdePreludat

data Animal = Animal {
  nombreAnimal :: String,
  tipoAnimal :: String,
  peso :: Number,
  edad :: Number,
  estaEnfermo :: Bool,
  visitasMedicas :: [VisitaMedica]
} deriving (Show, Eq)

data VisitaMedica = VisitaMedica {
  diasRecuperacion :: Number,
  costo :: Number
} deriving (Show, Eq)

-- Punto 1

laPasoMal :: Animal -> Bool
laPasoMal = any ((>30) . diasRecuperacion) . visitasMedicas

nombreFalopa :: Animal -> Bool
nombreFalopa animal = (=="i") . drop (subtract 1 . length . nombreAnimal $ animal) . nombreAnimal $ animal

-- Punto 2

type Actividad = Animal -> Animal

modificarPeso :: Number -> Animal -> Animal
modificarPeso kilos animal = animal {
  peso = (+kilos) . peso $ animal
}

engorde :: Number -> Actividad
engorde kilos = modificarPeso (min 5 . flip (/) 2 $ kilos)

revisacion :: VisitaMedica -> Actividad
revisacion visitaMedica animal
  | estaEnfermo animal = engorde 2 animal {
    visitasMedicas = (++[visitaMedica]) . visitasMedicas $ animal
  }
  | otherwise = animal

festejoCumple :: Actividad
festejoCumple animal = modificarPeso (-1) animal {
  edad = (+1) . edad $ animal
}

chequeoDePeso :: Number -> Actividad
chequeoDePeso pesoMinimo animal = animal {
  estaEnfermo = not ((>pesoMinimo) . peso $ animal) || estaEnfermo animal
}

-- Punto 3

type Proceso = [Actividad]

ejemploProceso :: Proceso
ejemploProceso = [engorde 4, revisacion (VisitaMedica 20 400), festejoCumple, chequeoDePeso 500]

aplicarProceso :: Proceso -> Actividad
aplicarProceso proceso animal = foldl (flip ($)) animal proceso

-- Ejemplo: aplicarProceso ejemploProceso dorothy

-- Punto 4

mejoraONoMejora :: Proceso -> Animal -> Bool
mejoraONoMejora [] _ = True
mejoraONoMejora (actividad:actividades) animal = mejoraSustentable animal actividad && (mejoraONoMejora actividades . actividad) animal

mejoraSustentable :: Animal -> Actividad -> Bool
mejoraSustentable animal actividad = rangoMejora . subtract (peso animal) . peso . actividad $ animal
  where rangoMejora diferenciaPeso = (>=0) diferenciaPeso && (<=3) diferenciaPeso

-- Punto 5

giveMeThree :: [Animal] -> [Animal]
giveMeThree = take 3 . filter nombreFalopa

-- Según la evaluación diferida, la función toma solo lo que necesita. En este caso, si la cantidad de animales que se le pasa a la función giveMeThree es infinita, s por lo tanto, la función convergería siempre y cuando logre encontrar tres animales con nombre falopa.
