--Dominio
type Edad = Int
type CantidadDeSuenios = Int
type Nombre = String
type Felicidonios = Int
type Habilidades = String
type Persona = (Edad, CantidadDeSuenios, Nombre, Felicidonios, [Habilidades])

-- Punto 1
-- Punto a)

pepe = (25,2,"Pepe",101,"Pintura")
pepito = (26,2,"Evangelina",100,"Ser buena persona")
pepita = (30,1,"Maximiliano",50,"Decir palindromos")

felicidonioPersona (_, _, _, felicidonio, _) = felicidonio

edadPersona (edad, _, _, _, _) = edad

cantidadDeSueniosPersona (_, cantidadDeSuenios, _, _, _) = cantidadDeSuenios

coeficienteDeSatisfaccion persona
    | felicidonioPersona persona > 100 = felicidonioPersona persona * edadPersona persona
    | felicidonioPersona persona <= 100 && felicidonioPersona persona > 50 = cantidadDeSueniosPersona persona * felicidonioPersona persona
    | otherwise = div (felicidonioPersona persona) 2

-- Punto b)

gradoDeAmbicion persona 
    | felicidonioPersona persona > 100 = felicidonioPersona persona * cantidadDeSueniosPersona persona
    | felicidonioPersona persona <= 100 && felicidonioPersona persona > 50 = edadPersona persona *cantidadDeSueniosPersona persona
    | otherwise = (cantidadDeSueniosPersona persona) * 2

-- Punto 2
-- Punto a)

nombrePersona (_, _, nombre, _, _) = nombre


--tieneNombreLargo10 = (>10).length.nombrePersona

-- Punto b)
pepenosuertudo = (25,2,"Pepenosuertudo",14,"Pintura")
pepesuertudo = (25,2,"Pepesuertudo",12,"Pintura")

personaSuertuda = even.(*3).coeficienteDeSatisfaccion

-- Punto c)
melina = (30,1,"Melina",50,"Decir palindromos")
ariel = (30,1,"Ariel",50,"Decir palindromos")
tieneNombreLindo = (=='a').last.nombrePersona
