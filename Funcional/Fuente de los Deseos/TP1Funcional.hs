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

-- Punto 1
-- Punto a: Coeficiente de satisfaccion
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
gradoAmbicion unaPersona
	| felicidoniosPersona unaPersona > 100 = (felicidoniosPersona unaPersona) * (cantidadSuenosPersona unaPersona)
	| (felicidoniosPersona unaPersona <= 100) && (felicidoniosPersona unaPersona > 50) = (edadPersona unaPersona) * (cantidadSuenosPersona unaPersona)
	| otherwise = (cantidadSuenosPersona unaPersona) * 2

-- Punto 2
-- Punto a: Nombre largo
nombreLargo (_, _, nombre, _, _) = length nombre > 10

nombrePersona (_, _, nombre, _, _) = nombre
nombreLargo unaPersona = length (nombrePersona unaPersona) > 10

nombreLargo = (length . nombrePersona) > 10

nombreLargo = (> 10) . length . nombrePersona

