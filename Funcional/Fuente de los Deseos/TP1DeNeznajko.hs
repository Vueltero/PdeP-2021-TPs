--Dominio
type Nombre = String
type Edad = Int
type CantidadDeSuenios = Int
type Felicidonios = Int
type Habilidades = [String]
type Persona = (Nombre, Edad, CantidadDeSuenios, Felicidonios, Habilidades)
--Funciones de desempaquetado
nombre :: Persona -> Nombre
nombre (nombreDePersona ,_ ,_ ,_ ,_ )=nombreDePersona
edad :: Persona -> Edad
edad (_,edadPersona,_,_,_)=edadPersona 
cantidadDeSuenios :: Persona -> CantidadDeSuenios
cantidadDeSuenios (_,_,cantidadDeSueniosPersona,_,_)=cantidadDeSueniosPersona
felicidonios :: Persona -> Felicidonios
felicidonios (_,_,_,nivelFelicidadPersona,_)=nivelFelicidadPersona
habilidades :: Persona -> Habilidades
habilidades (_,_,_,_,habilidadesPersona)=habilidadesPersona
--CasosDePrueba
evangelina :: Persona
evangelina=("Evangelina",25,2,101,["Cocinar estofado"])
maximiliano :: Persona
maximiliano=("Maximiliano",26,2,100,["Programar palíndromos"])
ariel :: Persona
ariel=("Ariel",102,1,50,["Natación"])
melina :: Persona
melina=("Melina",16,5,14,["Ninguna"])
tomas :: Persona
tomas=("Tomas",19,3,12,["Ser buena persona"])
--Punto 1a
coeficienteDeSatisfaccion :: Persona -> Int
coeficienteDeSatisfaccion persona
    |(felicidonios persona)>100=(felicidonios persona)*(edad persona) 
    |(felicidonios persona)<=100 && (felicidonios persona)>50=(felicidonios persona)*(cantidadDeSuenios persona)
    |otherwise=div (felicidonios persona) 2
--Punto 1b
gradoDeAmbicion :: Persona -> Int
gradoDeAmbicion persona
    |(felicidonios persona)>100=(felicidonios persona)*(cantidadDeSuenios persona) 
    |(felicidonios persona)<=100 && (felicidonios persona)>50=(edad persona)*(cantidadDeSuenios persona)
    |otherwise=(cantidadDeSuenios persona)*2
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
agregarFelicidonios numero persona=(nombre persona, edad persona, cantidadDeSuenios persona, (felicidonios persona)+numero, habilidades persona)
agregarHabilidad :: String -> Persona -> Persona
agregarHabilidad carrera persona=(nombre persona, edad persona, cantidadDeSuenios persona, felicidonios persona, carrera : (habilidades persona))
envejecer :: Persona -> Persona
envejecer persona=(nombre persona, (edad persona)+1, cantidadDeSuenios persona, felicidonios persona, habilidades persona)
--Resolusión
recibirseDeUnaCarrera :: String -> Persona -> Persona
recibirseDeUnaCarrera carrera=(agregarHabilidad carrera).(agregarFelicidonios.(*1000).length)carrera
viajarListaDeCiudades :: [String]-> Persona ->Persona
viajarListaDeCiudades ciudades = (envejecer).(agregarFelicidonios.(*100).length)ciudades
enamorarseDeOtraPersona :: Persona -> Persona -> Persona
enamorarseDeOtraPersona personaEnamorada personaX=(nombre personaEnamorada, edad personaEnamorada, cantidadDeSuenios personaEnamorada, (felicidonios personaEnamorada)+(felicidonios personaX), habilidades personaEnamorada)
queTodoSigaIgual :: Persona -> Persona
queTodoSigaIgual= id
comboPerfecto :: Persona -> Persona
comboPerfecto = (recibirseDeUnaCarrera "Medicina").(viajarListaDeCiudades ["Berazategui","Paris"]).(agregarFelicidonios 100)