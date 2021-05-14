---------------------------------------------------Dominio--------------------------------------------------
type Edad = Int
type CantidadDeSueños = Int
type Nombre = String
type Felicidonios = Int 
type Habilidades = [String]
type Persona = (Edad, CantidadDeSueños, Nombre, Felicidonios, Habilidades)
----------------------------------------------Desencapsulamiento---------------------------------------------------

edad:: Persona -> Edad
edad (edadPersona,_,_,_,_)= edadPersona

cantidadDeSueños:: Persona -> CantidadDeSueños
cantidadDeSueños (_,sueñosPersona,_,_,_)= sueñosPersona

nombre:: Persona -> Nombre
nombre (_,_,nombrePersona,_,_)= nombrePersona

felicidonios:: Persona -> Felicidonios
felicidonios (_,_,_,felicidoniosPersona,_)= felicidoniosPersona

habilidades:: Persona -> Habilidades
habilidades (_,_,_,_,habilidadesPersona)= habilidadesPersona
-----------------------------------------------Casos de prueba--------------------------------------------------

evangelina:: Persona
evangelina = (25,2,"Evangelina",101,["Comer", "Planchar"])

maximiliano::Persona
maximiliano = (26,2,"Maximiliano",100,["Atender","Jugar"])

jorge::Persona
jorge = (20,1,"Jorge Rial",50,["Hablar","Jugar"])

ariel::Persona
ariel = (24,3,"Ariel",14,["Leer","Mirar TV"])

melina::Persona
melina = (26,2,"Melina",12,["cambiar de ropa","Mirar TV"])

-------------------------------------------------(Punto 1)----------------------------------------------------
--(a)
coeficienteDeSastifaccion :: Persona -> Int 
coeficienteDeSastifaccion persona
    | felicidonios persona > 100 = felicidonios persona * edad persona
    | felicidonios  persona> 50 = cantidadDeSueños persona * felicidonios persona
    | otherwise = div (felicidonios persona) 2

--(b)
gradoDeAmbicion:: Persona -> Int
gradoDeAmbicion persona
    | felicidonios persona > 100 = felicidonios persona * cantidadDeSueños persona
    | felicidonios persona > 50 = edad persona * cantidadDeSueños persona
    | otherwise = cantidadDeSueños persona * 2 
-------------------------------------------------(Punto 1)----------------------------------------------------

-------------------------------------------------(Punto 2)----------------------------------------------------
--(a)
nombreLargo :: Persona -> Bool
nombreLargo = (>10).length.nombre 

--(b)
esPersonaSuertuda:: Persona -> Bool
esPersonaSuertuda = even.(*3).coeficienteDeSastifaccion

--(c)
esNombreLindo:: Persona -> Bool
esNombreLindo = (=='a').last.nombre
-------------------------------------------------(Punto 2)----------------------------------------------------

-------------------------------------------------(Punto 3)----------------------------------------------------
--(auxiliares)
agregarHabilidad:: String -> Persona -> Persona
agregarHabilidad carrera persona  = (edad persona, cantidadDeSueños persona,
 nombre persona, felicidonios persona, carrera : habilidades persona)

agregarFelicidonios:: Int -> Persona -> Persona
agregarFelicidonios bonusFelicidonios persona  = (edad persona, cantidadDeSueños persona,
 nombre persona, felicidonios persona + bonusFelicidonios, habilidades persona)

agregarAño:: Persona -> Persona
agregarAño persona = (edad persona + 1, cantidadDeSueños persona,
 nombre persona, felicidonios persona, habilidades persona)

---
recibirseDeUnaCarrera:: String -> Persona -> Persona
recibirseDeUnaCarrera carrera = agregarHabilidad carrera. agregarFelicidonios( length carrera *1000)

viajarACiudades:: [String] -> Persona -> Persona
viajarACiudades ciudades = agregarAño.agregarFelicidonios(length ciudades * 100)

enamorarseDePersona:: Persona -> Persona -> Persona
enamorarseDePersona persona2 = agregarFelicidonios(felicidonios persona2) 

todoSigueIgual::Persona -> Persona
todoSigueIgual persona = persona

comboPerfecto::  Persona -> Persona
comboPerfecto  = recibirseDeUnaCarrera "Medicina"
    .viajarACiudades ["Paris", "Berazategui"]
    .agregarFelicidonios 100 
-------------------------------------------------(Punto 3)-----------------------------------------
