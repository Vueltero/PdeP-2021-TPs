% Las Materias

materia(analisis_Matematico_I, 5).
materia(algebra_y_Geometria_Analitica, 5).
materia(matematica_Discreta, 3).
materia(sistemas_y_Organizaciones, 3).
materia(algoritmos_y_Estructuras_de_Datos, 5).
materia(arquitectura_de_Computadoras, 4).
materia(ingenieria_y_Sociedad, 2).
materia(quimica, 3).
materia(fisica_I, 5).
materia(analisis_Matematico_II, 5).
materia(probabilidad_y_Estadistica, 3).
materia(analisis_de_Sistemas, 6).
materia(sintaxis_y_Semantica_de_los_Lenguajes, 4).
materia(paradigmas_de_Programacion, 4).
materia(ingles_I, 2).
materia(sistemas_de_Representacion, 3).
materia(sistemas_Operativos, 4).
materia(disenio_de_Sistemas, 6).
materia(fisica_II, 5).
materia(matematica_Superior, 4).
materia(gestion_de_Datos, 4).
materia(legislacion, 2).
materia(economia, 3).
materia(ingles_II, 2).
materia(redes_de_Informacion, 4).
materia(administracion_de_Recursos, 6).
materia(investigacion_Operativa, 5).
materia(simulacion, 4).
materia(ingenieria_de_Software, 3).
materia(teoria_de_Control, 3).
materia(comunicaciones, 4).
materia(proyecto_Final, 6).
materia(inteligencia_Artificial, 3).
materia(administracion_Gerencial, 3).
materia(sistemas_de_Gestion, 4).

integradora(sistemas_y_Organizaciones).
integradora(analisis_de_Sistemas).
integradora(disenio_de_Sistemas).
integradora(administracion_de_Recursos).
integradora(proyecto_Final).

correlativa(analisis_de_Sistemas, sistemas_y_Organizaciones).
correlativa(analisis_de_Sistemas, algoritmos_y_Estructuras_de_Datos).
correlativa(analisis_Matematico_II, analisis_Matematico_I).
correlativa(analisis_Matematico_II, algebra_y_Geometria_Analitica).
correlativa(sintaxis_y_Semantica_de_los_Lenguajes, matematica_Discreta).
correlativa(sintaxis_y_Semantica_de_los_Lenguajes, algoritmos_y_Estructuras_de_Datos).
correlativa(paradigmas_de_Programacion, matematica_Discreta).
correlativa(paradigmas_de_Programacion, algoritmos_y_Estructuras_de_Datos).
correlativa(probabilidad_y_Estadistica, analisis_Matematico_I).
correlativa(probabilidad_y_Estadistica, algebra_y_Geometria_Analitica).
correlativa(disenio_de_Sistemas, analisis_de_Sistemas).
correlativa(disenio_de_Sistemas, paradigmas_de_Programacion).
correlativa(sistemas_Operativos, matematica_Discreta).
correlativa(sistemas_Operativos, algoritmos_y_Estructuras_de_Datos).
correlativa(sistemas_Operativos, arquitectura_de_Computadoras).
correlativa(fisica_II, analisis_Matematico_I).
correlativa(fisica_II, fisica_I).
correlativa(economia, analisis_de_Sistemas).
correlativa(gestion_de_Datos, analisis_de_Sistemas).
correlativa(gestion_de_Datos, paradigmas_de_Programacion).
correlativa(gestion_de_Datos, sintaxis_y_Semantica_de_los_Lenguajes).
correlativa(ingles_II, ingles_I).
correlativa(matematica_Superior, analisis_Matematico_II).
correlativa(legislacion, analisis_de_Sistemas).
correlativa(legislacion, ingenieria_y_Sociedad).
correlativa(administracion_de_Recursos, disenio_de_Sistemas).
correlativa(administracion_de_Recursos, sistemas_Operativos).
correlativa(administracion_de_Recursos, economia).
correlativa(ingenieria_de_Software, probabilidad_y_Estadistica).
correlativa(ingenieria_de_Software, disenio_de_Sistemas).
correlativa(ingenieria_de_Software, gestion_de_Datos).
correlativa(teoria_de_Control, quimica).
correlativa(teoria_de_Control, matematica_Superior).
correlativa(comunicaciones, arquitectura_de_Computadoras).
correlativa(comunicaciones, analisis_Matematico_II).
correlativa(comunicaciones, fisica_II).
correlativa(redes_de_Informacion, sistemas_Operativos).
correlativa(redes_de_Informacion, comunicaciones).
correlativa(investigacion_Operativa, probabilidad_y_Estadistica).
correlativa(investigacion_Operativa, matematica_Superior).
correlativa(simulacion, probabilidad_y_Estadistica).
correlativa(simulacion, matematica_Superior).
correlativa(inteligencia_Artificial, investigacion_Operativa).
correlativa(inteligencia_Artificial, simulacion).
correlativa(administracion_Gerencial, administracion_de_Recursos).
correlativa(administracion_Gerencial, investigacion_Operativa).
correlativa(sistemas_de_Gestion, administracion_de_Recursos).
correlativa(sistemas_de_Gestion, investigacion_Operativa).
correlativa(sistemas_de_Gestion, simulacion).
correlativa(proyecto_Final, legislacion).
correlativa(proyecto_Final, administracion_de_Recursos).
correlativa(proyecto_Final, redes_de_Informacion).
correlativa(proyecto_Final, ingenieria_de_Software).

% 1
esPesada(Materia):-
    materia(Materia, 6),
    integradora(Materia).

esPesada(Materia):-
    materia(Materia, Horas),
    not(integradora(Materia)),
    Horas >= 4.

% 2

% A
esMateriaInicial(Materia):-
    materia(Materia, _),
    not(correlativa(Materia, _)).

% B
correlativasMateria(Materia, CorrelativaDirecta):-
    correlativa(Materia, CorrelativaDirecta).

correlativasMateria(Materia, CorrelativaIndirecta):-
    correlativa(Materia, CorrelativaDirecta),
    correlativasMateria(CorrelativaDirecta, CorrelativaIndirecta).

% C
materiasQueHabilita(Materia, Habilita):- correlativasMateria(Habilita, Materia).

% Lxs Estudiantes

% 3
curso(vero, Materia, 8, anual, 2019):- esMateriaInicial(Materia).
curso(alan, sistemas_y_Organizaciones, 6, anual, 2018).
curso(alan, analisis_Matematico_I, 6, anual, 2018).
curso(alan, analisis_de_Sistemas, 2, anual, 2019).
curso(alan, analisis_de_Sistemas, 9, anual, 2020).
curso(alan, fisica_I, 2, anual, 2019).

curso(jazmin, sistemas_y_Organizaciones, 6, anual, 2015). %6
curso(jazmin, quimica, 4, cuatrimestral(1), 2015). %3
curso(jazmin, quimica, 8, cuatrimestral(2), 2015). %6
curso(jazmin, fisica_I, 8, curso_de_Verano, 2016). %5

curso(muchasRecursadas, quimica, 2, anual, 2016).
curso(muchasRecursadas, quimica, 3, cuatrimestral(1), 2017).
curso(muchasRecursadas, quimica, 4, cuatrimestral(2), 2017).
curso(muchasRecursadas, quimica, 5, anual, 2018).
curso(muchasRecursadas, fisica_I, 2, curso_de_Verano, 2018).
curso(muchasRecursadas, fisica_I, 2, anual, 2018).

curso(algunasRecursadas, quimica, 2, anual, 2016).
curso(algunasRecursadas, quimica, 3, cuatrimestral(2), 2017).
curso(algunasRecursadas, fisica_I, 2, anual, 2017).
curso(algunasRecursadas, fisica_I, 10, cuatrimestral(1), 2018).

curso(veraniego, quimica, 6, anual, 2016). %6
curso(veraniego, fisica_I, 6, curso_de_Verano, 2017). %3
curso(veraniego, matematica_Discreta, 2, anual, 2017). %2
curso(veraniego, matematica_Discreta, 8, curso_de_Verano, 2018). %5

curso(atr, quimica, 10, cuatrimestral(1), 2016).
curso(atr, fisica_I, 10, cuatrimestral(2), 2016).

rindioFinal(alan, sistemas_y_Organizaciones, 4).
rindioFinalLibre(vero, ingles_II, 10).
rindioFinalLibre(alan, ingles_I, 2).

notaDeCursada(Estudiante, Materia, Nota):- curso(Estudiante, Materia, Nota, _, _).
aprobo(Nota):- Nota > 5.
promociono(Nota):- Nota > 7.
aproboLibre(Estudiante, Materia):- rindioFinalLibre(Estudiante, Materia, Nota), aprobo(Nota).

% A
materiasCursadas(Estudiante, Materia):-
    notaDeCursada(Estudiante, Materia, Nota),
    aprobo(Nota).

materiasCursadas(Estudiante, Materia):-
    aproboLibre(Estudiante, Materia).

% B
materiasAprobadas(Estudiante, Materia):-
    rindioFinal(Estudiante, Materia, Nota),
    aprobo(Nota).

% Habría que preguntar si la cursó y aprobó

materiasAprobadas(Estudiante, Materia):-
    aproboLibre(Estudiante, Materia).

materiasAprobadas(Estudiante, Materia):-
    notaDeCursada(Estudiante, Materia, Nota),
    promociono(Nota).

% Modalidades

% 4
anioMateriaCursada(Estudiante, Materia, curso_de_Verano, Anio):-
    curso(Estudiante, Materia, _, curso_de_Verano, AnioCalendario),
    Anio is AnioCalendario - 1.

anioMateriaCursada(Estudiante, Materia, Modalidad, Anio):-
    curso(Estudiante, Materia, _, Modalidad, Anio),
    Modalidad \= curso_de_Verano.

% 5
materiasRecursadas(Estudiante, Materia):-
    curso(Estudiante, Materia, _, _, Anio1),
    curso(Estudiante, Materia, _, _, Anio2),
    Anio1 \= Anio2.

materiasRecursadas(Estudiante, Materia):-
    curso(Estudiante, Materia, _, cuatrimestral(1), Anio),
    curso(Estudiante, Materia, _, cuatrimestral(2), Anio).

% Perfiles de estudiantes

% 6

estudiante(Estudiante):- curso(Estudiante, _, _, _, _).

% A
recursoInmediatamente(Estudiante, cuatrimestral(1), cuatrimestral(2)):-
    anioMateriaCursada(Estudiante, Materia, cuatrimestral(1), Anio),
    anioMateriaCursada(Estudiante, Materia, cuatrimestral(2), Anio).

recursoInmediatamente(Estudiante, Modalidad1, Modalidad2):-
    anioMateriaCursada(Estudiante, Materia, Modalidad1, Anio),
    anioMateriaCursada(Estudiante, Materia, Modalidad2, AnioSiguiente),
    AnioSiguiente is Anio + 1.

sinDescanso(Estudiante):-
    recursoInmediatamente(Estudiante, cuatrimestral(1), cuatrimestral(2)).

sinDescanso(Estudiante):-
    recursoInmediatamente(Estudiante, cuatrimestral(2), anual).

sinDescanso(Estudiante):-
    recursoInmediatamente(Estudiante, anual, anual).

sinDescanso(Estudiante):-
    recursoInmediatamente(Estudiante, anual, cuatrimestral(1)).

% B
invictus(Estudiante):-
    estudiante(Estudiante),
    not(materiasRecursadas(Estudiante, _)).

% C
repechaje(Estudiante):-
    curso(Estudiante, Materia, Nota, anual, Anio),
    not(aprobo(Nota)),
    curso(Estudiante, Materia, Nota2, cuatrimestral(1), AnioSiguiente),
    AnioSiguiente is Anio + 1,
    promociono(Nota2).

% D
buenasCursadas(Estudiante):-
    estudiante(Estudiante),
    forall(
        notaDeCursada(Estudiante, _, Nota),
        promociono(Nota)
        ).

% E
materiaNoCursadaEnVerano(Estudiante, Anio):-
    curso(Estudiante, _, _, Modalidad, Anio),
    Modalidad \= curso_de_Verano.

seLoQueHicisteElVeranoPasado(Estudiante):-
    estudiante(Estudiante),
    forall(
        materiaNoCursadaEnVerano(Estudiante, Anio),
        anioMateriaCursada(Estudiante, _, curso_de_Verano, Anio)
        ).

% Desempeño academico

% 8

anioPar(Anio):- 0 is Anio mod 2.
anioImpar(Anio):- not(anioPar(Anio)).

calculoNota(Estudiante, Nota):-
    curso(Estudiante, _, Nota, anual, _).

calculoNota(Estudiante, Nota):-
    curso(Estudiante, _, NotaCursada, cuatrimestral(Cuatrimestre), _),
    Nota is NotaCursada - Cuatrimestre.

calculoNota(Estudiante, 5):-
    curso(Estudiante, _, _, curso_de_Verano, Anio),
    anioPar(Anio).

calculoNota(Estudiante, Nota):-
    curso(Estudiante, _, NotaCursada, curso_de_Verano, Anio),
    anioImpar(Anio),
    Nota is NotaCursada // 2.

todasLasNotas(Estudiante, Notas):-
    estudiante(Estudiante),
    findall(Nota, calculoNota(Estudiante, Nota), Notas).

desempenioAcademico(Estudiante, PromedioTotal):-
    todasLasNotas(Estudiante, Notas),
    sumlist(Notas, SumaNotas),
    length(Notas, CantidadNotas),
    PromedioTotal is SumaNotas / CantidadNotas.