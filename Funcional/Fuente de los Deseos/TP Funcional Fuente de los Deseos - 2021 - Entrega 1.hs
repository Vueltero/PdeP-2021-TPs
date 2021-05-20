---------------------------------------------------Dominio--------------------------------------------------
data Persona = Persona {
    nombre :: String,
    edad :: Int,
    sueños :: Int,
    felicidonios :: Int,
    habilidades :: [String]
} deriving Show

---------------------------------------------Casos de prueba------------------------------------------------
evangelina :: Persona
evangelina = Persona "Evangelina" 25 2 101 ["Pintura"]

maximiliano :: Persona
maximiliano = Persona "Maximiliano" 26 2 100 ["Ser buena persona"]

ariel :: Persona
ariel = Persona "Ariel" 30 1 50 ["Decir palíndromos"]

melina :: Persona
melina = Persona "Melina" 17 1 14 ["Levantar una ceja"]

tomas :: Persona
tomas = Persona "Tomas" 19 3 12 ["Natación"]

-----------------------------------------------(Punto 1)----------------------------------------------------
muyFeliz :: Persona -> Bool
muyFeliz = (>100) . felicidonios

moderadamenteFeliz :: Persona -> Bool
moderadamenteFeliz = (>50) . felicidonios 

pocoFeliz :: Persona -> Bool
pocoFeliz = (>0) . felicidonios

funcionAuxiliar :: (Persona -> Int) -> (Persona -> Int) -> (Persona -> Int) -> (Int -> Int -> Int) -> Persona -> Int
funcionAuxiliar funcion1 funcion2 funcion3 funcion4 persona
    | muyFeliz persona           = funcion1 persona * funcion2 persona
    | moderadamenteFeliz persona = funcion1 persona * funcion3 persona
    | pocoFeliz persona          = funcion4 (funcion1 persona) 2

--(a)
coeficienteDeSatisfaccion :: Persona -> Int
coeficienteDeSatisfaccion = funcionAuxiliar felicidonios edad sueños div
--coeficienteDeSatisfaccion persona
--  | muyFeliz persona =           felicidonios persona * edad persona
--  | moderadamenteFeliz persona = felicidonios persona * sueños persona
--  | pocoFeliz persona =          div (felicidonios persona) 2

--(b)
gradoDeAmbicion :: Persona -> Int
gradoDeAmbicion = funcionAuxiliar sueños felicidonios edad (*)
--gradoDeAmbicion persona
--  | muyFeliz persona =           sueños persona * felicidonios persona
--  | moderadamenteFeliz persona = sueños persona * edad persona
--  | pocoFeliz persona =          (*) (sueños persona) 2

-----------------------------------------------(Punto 2)----------------------------------------------------
--(a)
nombreLargo :: Persona -> Bool
nombreLargo = (>10) . length . nombre

--(b)
esPersonaSuertuda :: Persona -> Bool
esPersonaSuertuda = even . (*3) . coeficienteDeSatisfaccion

--(c)
esNombreLindo :: Persona -> Bool
esNombreLindo = (=='a') . last . nombre

-----------------------------------------------(Punto 3)----------------------------------------------------
--Funciones Auxiliares
agregarFelicidonios :: Int -> Persona -> Persona
agregarFelicidonios numero persona = persona {
    felicidonios = felicidonios persona + numero
}

agregarHabilidad :: String -> Persona -> Persona
agregarHabilidad habilidad persona = persona {
    habilidades = habilidad : habilidades persona
}

envejecer :: Persona -> Persona
envejecer persona = persona {
    edad = edad persona + 1
}

sueño :: (Persona -> Persona) -> Int -> Persona -> Persona
sueño funcion numero = funcion . agregarFelicidonios numero

--Funciones Principales
recibirseDeUnaCarrera :: String -> Persona -> Persona
recibirseDeUnaCarrera carrera = sueño (agregarHabilidad carrera) (length carrera * 1000)

viajarACiudades :: [String] -> Persona -> Persona
viajarACiudades ciudades = sueño envejecer (length ciudades * 100)

enamorarseDePersona :: Persona -> Persona -> Persona
enamorarseDePersona = flip (sueño id . felicidonios)
--enamorarseDePersona enamorado persona = sueño id (felicidonios persona) enamorado

queTodoSigaIgual :: Persona -> Persona
queTodoSigaIgual = id

comboPerfecto :: Persona -> Persona
comboPerfecto = sueño
    (recibirseDeUnaCarrera "Medicina" . viajarACiudades ["Berazategui", "París"])
    100