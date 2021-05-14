--Dominio
type Nombre = String
type Edad = Int
type CantidadDeSueños = Int
type Felicidonios = Int
type Habilidades = [String]

type Persona = (Nombre, Edad, CantidadDeSueños, Felicidonios, Habilidades)

--Funciones accessor
nombre :: Persona -> Nombre
nombre (nombrePersona, _, _, _, _) = nombrePersona

edad :: Persona -> Edad
edad (_, edadPersona, _, _, _) = edadPersona

sueños :: Persona -> CantidadDeSueños
sueños (_, _, sueñosPersona, _, _) = sueñosPersona

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
melina = ("Melina", 17, 1, 14, ["Levantar una ceja"])

tomas :: Persona
tomas = ("Tomas", 19, 3, 12, ["Natación"])

--Punto 1
coeficienteDeSatisfaccion :: Persona -> Int
coeficienteDeSatisfaccion persona
    | felicidonios persona > 100 = felicidonios persona * edad persona
    | felicidonios persona > 50  = felicidonios persona * sueños persona
    | otherwise = div (felicidonios persona) 2

gradoDeAmbicion :: Persona -> Int
gradoDeAmbicion persona
    | felicidonios persona > 100 = sueños persona * felicidonios persona
    | felicidonios persona > 50  = sueños persona * edad persona
    | otherwise = sueños persona * 2

--Punto 2
nombreLargo :: Persona -> Bool
nombreLargo = (>10) . length . nombre

suertuda :: Persona -> Bool
suertuda = even . (*3) . coeficienteDeSatisfaccion

nombreLindo :: Persona -> Bool
nombreLindo = (=='a') . last . nombre

--Funciones auxiliares punto 3
agregarFelicidonios :: Felicidonios -> Persona -> Persona
agregarFelicidonios numero persona = (
    nombre persona,
    edad persona,
    sueños persona,
    felicidonios persona + numero,
    habilidades persona
    )

agregarHabilidad :: String -> Persona -> Persona
agregarHabilidad habilidad persona = (
    nombre persona,
    edad persona,
    sueños persona,
    felicidonios persona,
    habilidad : habilidades persona
    )

envejecer :: Persona -> Persona
envejecer persona = (
    nombre persona,
    edad persona + 1,
    sueños persona,
    felicidonios persona,
    habilidades persona
    )

--Punto 3
recibirseDeUnaCarrera :: String -> Persona -> Persona
recibirseDeUnaCarrera carrera = agregarHabilidad carrera
    . agregarFelicidonios (length carrera * 1000)

viajar :: [String] -> Persona -> Persona
viajar ciudades = envejecer
    . agregarFelicidonios (length ciudades * 100)

enamorarse :: Persona -> Persona -> Persona
enamorarse = flip (agregarFelicidonios . felicidonios)

queTodoSigaIgual :: Persona -> Persona
queTodoSigaIgual = id

comboPerfecto :: Persona -> Persona
comboPerfecto = recibirseDeUnaCarrera "Medicina"
    . viajar ["Berazategui", "París"]
    . agregarFelicidonios 100