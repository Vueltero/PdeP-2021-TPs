--Dominio
data Persona = Persona {
    nombre :: String
    edad :: Int
    cantidadDeSueños :: Int
    felicidonios :: Int
    habilidades :: [String]
} deriving Show
--CasosDePrueba
evangelina :: Persona
evangelina= "Evangelina" 25 2 101 ["Cocinar estofado"]
maximiliano :: Persona
maximiliano= "Maximiliano" 26 2 100 ["Programar palíndromos"]
ariel :: Persona
ariel= "Ariel" 102 1 50 ["Natación"]
melina :: Persona
melina= "Melina" 16 5 14 ["Ninguna"]
tomas :: Persona
tomas= "Tomas" 19 3 12 ["Ser buena persona"]
-- Punto 1 
-- Funciones auxiliares
muyFeliz :: Persona -> Bool
muyFeliz = (>100).felicidonios
moderadamenteFeliz :: Persona -> Bool
moderadamenteFeliz = (>50).felicidonios
pocoFeliz :: Persona -> Bool
pocoFeliz = (>0).felicidonios
operacionesPosibles :: (Persona -> Int) -> (Persona -> Int) -> (Persona -> Int) -> (Int -> Int) -> Int
operacionesPosibles funcion1 funcion2 funcion3 funcion4 = 
    |muyFeliz persona=funcion1*funcion2
    

--Punto 1a
coeficienteDeSatisfaccion :: Persona -> Int
coeficienteDeSatisfaccion persona = operacionesPosibles felicidonios edad cantidadDeSueños div
    |muyFeliz persona=(felicidonios persona)*(edad persona) 
    |moderadamenteFeliz persona=(felicidonios persona)*(cantidadDeSueños persona)
    |otherwise=div (felicidonios persona) 2
--Punto 1b
gradoDeAmbicion :: Persona -> Int
gradoDeAmbicion persona
    |muyFeliz=(cantidadDeSueños
 persona)*(felicidonios persona)
    |moderadamenteFeliz=(cantidadDeSueños
 persona)*(edad persona)
    |otherwise=(cantidadDeSueños
 persona)*2
--Punto 2a
nombreLargo :: Persona -> Bool
nombreLargo=(>10).length.nombre
--Punto 2b
personaSuertuda :: Persona -> Bool
personaSuertuda=even.(* 3).coeficienteDeSatisfaccion
--Punto 2c
nombreLindo :: Persona -> Bool
nombreLindo=(=='a').last.nombre
--Punto 3
--FuncionesAuxiliares
agregarFelicidonios :: Int -> Persona -> Persona
agregarFelicidonios numero persona=(nombre persona, edad persona, cantidadDeSueños persona, (felicidonios persona)+numero, habilidades persona)
agregarHabilidad :: String -> Persona -> Persona
agregarHabilidad carrera persona=(nombre persona, edad persona, cantidadDeSueños persona, felicidonios persona, carrera : (habilidades persona))
envejecer :: Persona -> Persona
envejecer persona=(nombre persona, (edad persona)+1, cantidadDeSueños persona, felicidonios persona, habilidades persona)
--Resolusión
recibirseDeUnaCarrera :: String -> Persona -> Persona
recibirseDeUnaCarrera carrera=(agregarHabilidad carrera).(agregarFelicidonios.(*1000).length)carrera
viajarListaDeCiudades :: [String]-> Persona ->Persona
viajarListaDeCiudades ciudades = (envejecer).(agregarFelicidonios.(*100).length)ciudades
enamorarseDeOtraPersona :: Persona -> Persona -> Persona
enamorarseDeOtraPersona personaEnamorada personaX=(nombre personaEnamorada, edad personaEnamorada, cantidadDeSueños personaEnamorada, (felicidonios personaEnamorada)+(felicidonios personaX), habilidades personaEnamorada)
queTodoSigaIgual :: Persona -> Persona
queTodoSigaIgual= id
comboPerfecto :: Persona -> Persona
comboPerfecto = (recibirseDeUnaCarrera "Medicina").(viajarListaDeCiudades ["Berazategui","Paris"]).(agregarFelicidonios 100)