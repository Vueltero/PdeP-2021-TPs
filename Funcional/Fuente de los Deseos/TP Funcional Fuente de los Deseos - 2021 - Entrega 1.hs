---------------------------------------------------Dominio--------------------------------------------------
type Nombre = String
type Edad = Int
type CantidadDeSueños = Int
type Felicidonios = Int
type Habilidades = [String]
type Persona = (Nombre, Edad, CantidadDeSueños, Felicidonios, Habilidades)

----------------------------------------------Funciones accesor--------------------------------------------------
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

---------------------------------------------Casos de prueba--------------------------------------------------
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

-----------------------------------------------(Punto 1)----------------------------------------------------
--(a)
coeficienteDeSatisfaccion :: Persona -> Int
coeficienteDeSatisfaccion persona
    | felicidonios persona > 100 = felicidonios persona * edad persona
    | felicidonios persona > 50  = felicidonios persona * sueños persona
    | otherwise = div (felicidonios persona) 2

--(b)
gradoDeAmbicion :: Persona -> Int
gradoDeAmbicion persona
    | felicidonios persona > 100 = sueños persona * felicidonios persona
    | felicidonios persona > 50  = sueños persona * edad persona
    | otherwise = sueños persona * 2

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

--Funciones Principales
recibirseDeUnaCarrera :: String -> Persona -> Persona
recibirseDeUnaCarrera carrera = agregarHabilidad carrera
    . agregarFelicidonios (length carrera * 1000)

viajarACiudades :: [String] -> Persona -> Persona
viajarACiudades ciudades = envejecer
    . agregarFelicidonios (length ciudades * 100)

enamorarseDePersona :: Persona -> Persona -> Persona
enamorarseDePersona = flip (agregarFelicidonios . felicidonios)

queTodoSigaIgual :: Persona -> Persona
queTodoSigaIgual = id

comboPerfecto :: Persona -> Persona
comboPerfecto = recibirseDeUnaCarrera "Medicina"
    . viajarACiudades ["Berazategui", "París"]
    . agregarFelicidonios 100
