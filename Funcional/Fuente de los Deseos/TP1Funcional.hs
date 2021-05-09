-- TP Funcional - Paradigmas de Programacion
-- Fuente de los deseos

-- Dominio
type Edad = Int
type CantidadSuenios = Int
type Nombre = String
type Felicidonios = Int
type Habilidad = String
type Habilidades = [Habilidad]

type Persona = (Edad, CantidadSuenios, Nombre, Felicidonios, Habilidades)

--Casos de prueba
evangelina :: Persona
evangelina = (25, 2, "Evangelina", 101, ["Pintura"])

maximiliano :: Persona
maximiliano = (26, 2, "Maximiliano", 100, ["Ser buena persona"])

ariel :: Persona
ariel = (30, 1, "Ariel", 50, ["Decir palíndromos"])

melina :: Persona
melina = (17, 1, "Melina", 12, ["Levantar una ceja"])

-- Punto 1
-- Punto a: Coeficiente de satisfaccion
coeficienteSatisfaccion :: Persona -> Int
coeficienteSatisfaccion unaPersona
	| felicidoniosPersona unaPersona > 100 = (felicidoniosPersona unaPersona) * (edadPersona unaPersona)
	| (felicidoniosPersona unaPersona <= 100) && (felicidoniosPersona unaPersona > 50) = (cantidadSuenosPersona unaPersona) * (felicidoniosPersona unaPersona)
	| otherwise = div (felicidoniosPersona unaPersona) 2

felicidoniosPersona :: Persona -> Felicidonios
felicidoniosPersona (_, _, _, felicidonios, _) = felicidonios

edadPersona :: Persona -> Edad
edadPersona (edad, _, _, _, _) = edad

cantidadSuenosPersona :: Persona -> CantidadSuenios
cantidadSuenosPersona (_, cantidadSuenios, _, _, _) = cantidadSuenios

-- Punto b: Grado de ambicion de una persona
gradoAmbicion :: Persona -> Int
gradoAmbicion unaPersona
	| felicidoniosPersona unaPersona > 100 = (felicidoniosPersona unaPersona) * (cantidadSuenosPersona unaPersona)
	| (felicidoniosPersona unaPersona <= 100) && (felicidoniosPersona unaPersona > 50) = (edadPersona unaPersona) * (cantidadSuenosPersona unaPersona)
	| otherwise = (cantidadSuenosPersona unaPersona) * 2

-- Punto 2
-- Punto a: Nombre largo
nombrePersona:: Persona -> Nombre
nombrePersona (_, _, nombre, _, _) = nombre

nombreLargo :: Persona -> Bool
nombreLargo = (> 10) . length . nombrePersona

-- Punto b: Persona suertuda
personaSuertuda :: Persona -> Bool
personaSuertuda = even . (* 3) . coeficienteSatisfaccion

-- Punto c: Nombre lindo
nombreLindo :: Persona -> Bool
nombreLindo = (== 'a') . last . nombrePersona

-- Punto 3
agregarFelicidonios :: Persona -> Int -> Persona
agregarFelicidonios (edad, cantidadSuenios, nombre, felicidonios, habilidades) cantidad =
	(edad, cantidadSuenios, nombre, felicidonios + cantidad, habilidades)
--agregarFelicidonios (_, _, _, felicidonios, _) cantidad = (_, _, _, felicidonios + cantidad, _)

recibirseDeCarrera :: Persona -> String -> Persona
recibirseDeCarrera (edad, cantidadSuenios, nombre, felicidonios, habilidades) carrera =
	(edad, cantidadSuenios, nombre, felicidonios + (length carrera) * 1000, (:) carrera habilidades)

viajarAListaDeCiudades :: Persona -> [String] -> Persona
viajarAListaDeCiudades (edad, cantidadSuenios, nombre, felicidonios, habilidades) ciudades =
	(edad + 1, cantidadSuenios, nombre, felicidonios + (length ciudades) * 100, habilidades)

enamorarseDeOtraPersona :: Persona -> Persona -> Persona
enamorarseDeOtraPersona (edad, cantidadSuenios, nombre, felicidonios, habilidades) otraPersona =
	(edad, cantidadSuenios, nombre, felicidonios + felicidoniosPersona otraPersona, habilidades)

queTodoSigaIgual :: Persona -> Persona
queTodoSigaIgual = id

comboPerfecto :: Persona -> Persona
comboPerfecto unaPersona = agregarFelicidonios (viajarAListaDeCiudades (recibirseDeCarrera unaPersona "Medicina") ["Berazategui", "Paris"]) 100