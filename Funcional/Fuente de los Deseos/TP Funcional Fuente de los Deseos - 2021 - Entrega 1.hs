--Dominio
type Nombre = String
type Edad = Int
type CantidadDeSuenos = Int
type Felidocinios = Int
type Habilidades = [String]

type Persona = (Nombre, Edad, CantidadDeSuenos, Felidocinios, Habilidades)

--Funciones de desempaquetado
nombre :: Persona -> Nombre
nombre (nombre, _, _, _, _) = nombre

edad :: Persona -> Edad
edad (_, edad, _, _, _) = edad

suenos :: Persona -> CantidadDeSuenos
suenos (_, _, suenos, _, _) = suenos

felidocinios :: Persona -> Felidocinios
felidocinios (_, _, _, felidocinios, _) = felidocinios

habilidades  :: Persona -> Habilidades
habilidades (_, _, _, _, habilidades) = habilidades

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
    | (felidocinios persona) > 100 = (*) (felidocinios persona) (edad persona)
    | (felidocinios persona) > 50  = (*) (felidocinios persona) (suenos persona)
    | otherwise = div (felidocinios persona) 2

gradoDeAmbicion :: Persona -> Int
gradoDeAmbicion persona
    | (felidocinios persona) > 100 = (*) (suenos persona) (felidocinios persona)
    | (felidocinios persona) > 50  = (*) (suenos persona) (edad persona)
    | otherwise = (*) (suenos persona) 2

--Punto 2
nombreLargo :: Persona -> Bool
nombreLargo = (>10) . length . nombre

suertuda :: Persona -> Bool
suertuda = even . (*3) . coeficienteDeSatisfaccion

nombreLindo :: Persona -> Bool
nombreLindo = (== 'a') . last . nombre

--Punto 3
agregarFelidonicios :: Int -> Persona -> Persona
agregarFelidonicios numero persona =
    (nombre persona,
    edad persona,
    suenos persona,
    (felidocinios persona) + numero,
    --((+numero) . felidocinios) persona,
    habilidades persona)

agregarHabilidad :: String -> Persona -> Persona
agregarHabilidad habilidad persona =
    (nombre persona,
    edad persona,
    suenos persona,
    felidocinios persona,
    (:) habilidad (habilidades persona))

envejecer :: Persona -> Persona
envejecer persona =
    (nombre persona,
    (edad persona) + 1,
    --((+1) . edad) persona,
    suenos persona,
    felidocinios persona,
    habilidades persona)

recibirseDeUnaCarrera :: String -> Persona -> Persona
recibirseDeUnaCarrera carrera = (agregarHabilidad carrera)
    . (agregarFelidonicios . (*1000) . length) carrera

viajar :: [String] -> Persona -> Persona
viajar ciudades = envejecer
    . (agregarFelidonicios . (*100) . length) ciudades

enamorarse :: Persona -> Persona -> Persona
enamorarse persona1 persona2 = agregarFelidonicios (felidocinios persona2) persona1

queTodoSigaIgual :: Persona -> Persona
queTodoSigaIgual persona = persona

comboPerfecto :: Persona -> Persona
comboPerfecto = (recibirseDeUnaCarrera "Medicina")
    . (viajar ["Berazategui", "París"])
    . (agregarFelidonicios 100)