--Dominio
type Nombre = String
type Edad = Int
type CantidadDeSuenos = Int
type Felicidonios = Int
type Habilidades = [String]

type Persona = (Nombre, Edad, CantidadDeSuenos, Felicidonios, Habilidades)

--Funciones de desempaquetado
nombre :: Persona -> Nombre
nombre (nombrePersona, _, _, _, _) = nombrePersona

edad :: Persona -> Edad
edad (_, edadPersona, _, _, _) = edadPersona

suenos :: Persona -> CantidadDeSuenos
suenos (_, _, suenosPersona, _, _) = suenosPersona

felicidonios :: Persona -> Felicidonios
felicidonios (_, _, _, felicidoniosPersona, _) = felicidoniosPersona

habilidades  :: Persona -> Habilidades
habilidades (_, _, _, _, habilidadesPersona) = habilidadesPersona

--Casos de prueba
evangelina :: Persona
evangelina = ("Evangelina", 25, 2, 101, ["Pintura"])

maximiliano :: Persona
maximiliano = ("Maximiliano", 26, 2, 100, ["Ser buena persona"])

ariel :: Persona
ariel = ("Ariel", 30, 1, 50, ["Decir palíndromos"])

melina :: Persona
melina = ("Melina", 17, 1, 12, ["Levantar una ceja"])

--Punto 1
coeficienteDeSatisfaccion :: Persona -> Int
coeficienteDeSatisfaccion persona
    | (felicidonios persona) > 100 = (felicidonios persona) * (edad persona)
    | (felicidonios persona) > 50  = (felicidonios persona) * (suenos persona)
    | otherwise = div (felicidonios persona) 2

gradoDeAmbicion :: Persona -> Int
gradoDeAmbicion persona
    | (felicidonios persona) > 100 = (suenos persona) * (felicidonios persona)
    | (felicidonios persona) > 50  = (suenos persona) * (edad persona)
    | otherwise = (suenos persona) * 2

--Punto 2
nombreLargo :: Persona -> Bool
nombreLargo = (>10) . length . nombre

suertuda :: Persona -> Bool
suertuda = even . (*3) . coeficienteDeSatisfaccion

nombreLindo :: Persona -> Bool
nombreLindo = (== 'a') . last . nombre

--Funciones auxiliares punto 3
agregarFelidonicios :: Int -> Persona -> Persona
agregarFelidonicios numero persona =
    (nombre persona,
    edad persona,
    suenos persona,
    (felicidonios persona) + numero,
    habilidades persona)

agregarHabilidad :: String -> Persona -> Persona
agregarHabilidad habilidad persona =
    (nombre persona,
    edad persona,
    suenos persona,
    felicidonios persona,
    habilidad : (habilidades persona))

envejecer :: Persona -> Persona
envejecer persona =
    (nombre persona,
    (edad persona) + 1,
    suenos persona,
    felicidonios persona,
    habilidades persona)

--Punto 3
recibirseDeUnaCarrera :: String -> Persona -> Persona
recibirseDeUnaCarrera carrera = (agregarHabilidad carrera)
    . (agregarFelidonicios . (*1000) . length) carrera

viajar :: [String] -> Persona -> Persona
viajar ciudades = envejecer
    . (agregarFelidonicios . (*100) . length) ciudades

enamorarse :: Persona -> Persona -> Persona
enamorarse persona1 persona2 = agregarFelidonicios (felicidonios persona2) persona1

queTodoSigaIgual :: Persona -> Persona
queTodoSigaIgual = id

comboPerfecto :: Persona -> Persona
comboPerfecto = (recibirseDeUnaCarrera "Medicina")
    . (viajar ["Berazategui", "París"])
    . (agregarFelidonicios 100)