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
cantidadSegunTipoPersona :: (Persona -> Int) -> (Persona -> Int) -> (Persona -> Int) -> (Int -> Int -> Int) -> Persona -> Int
cantidadSegunTipoPersona funcion1 funcion2 funcion3 operador persona
    | muyFeliz           = caracteristicaPersona1 * caracteristicaPersona2
    | moderadamenteFeliz = caracteristicaPersona1 * caracteristicaPersona3
    | pocoFeliz          = operador (caracteristicaPersona1) 2

    where
        muyFeliz = (>100) . felicidonios $ persona
        moderadamenteFeliz = (>50) . felicidonios $ persona
        pocoFeliz = (>0) . felicidonios $ persona

        caracteristicaPersona1 = funcion1 persona
        caracteristicaPersona2 = funcion2 persona
        caracteristicaPersona3 = funcion3 persona

--(a)
coeficienteDeSatisfaccion :: Persona -> Int
coeficienteDeSatisfaccion = cantidadSegunTipoPersona felicidonios edad sueños div

--(b)
gradoDeAmbicion :: Persona -> Int
gradoDeAmbicion = cantidadSegunTipoPersona sueños felicidonios edad (*)

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