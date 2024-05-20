:- use_module(library(pce)).
:- pce_global(@pce, new(pce)).

% Datos de ejemplo
es_planta_medicinal(marihuana, general, "Si, se utiliza para fines recreativos y medicinales.").
es_planta_medicinal(marihuana, especifico, "Se usa para tratar dolor cronico, nauseas, y para estimular el apetito.").
es_planta_medicinal(mastuerzo, general, "Si, es una planta medicinal.").
es_planta_medicinal(mastuerzo, especifico, "Se utiliza para tratar enfermedades respiratorias y como antiseptico.").
es_planta_medicinal(matarique, general, "Si, se utiliza en la medicina tradicional.").
es_planta_medicinal(matarique, especifico, "Se usa como expectorante y antiinflamatorio para problemas respiratorios.").
es_planta_medicinal(menta, general, "Si, es una planta medicinal.").
es_planta_medicinal(menta, especifico, "Se utiliza para tratar problemas digestivos y como refrescante.").
es_planta_medicinal(girasol, general, "Si, se utiliza en la medicina tradicional.").
es_planta_medicinal(girasol, especifico, "Se usa para tratar dolor de estomago y nerviosismo.").
es_planta_medicinal(gingseng, general, "Si, es una planta medicinal.").
es_planta_medicinal(gingseng, especifico, "Se utiliza como panacea para mejorar la salud en general.").
es_planta_medicinal(gordolobo, general, "Si, se utiliza en la medicina tradicional.").
es_planta_medicinal(gordolobo, especifico, "Se usa para tratar la bronquitis.").
es_planta_medicinal(grama, general, "Si, se utiliza en la medicina tradicional.").
es_planta_medicinal(grama, especifico, "Se usa para tratar inflamaciones.").

elementos(marihuana, "THC, CBD, Terpenos").
elementos(mastuerzo, "Vitamina C, Glucosinolatos").
elementos(matarique, "Aceites esenciales, Flavonoides").
elementos(menta, "Mentol, Flavonoides, Acido rosmarinico").
elementos(girasol, "Acidos grasos, Vitamina E").
elementos(gingseng, "Ginsenosidos, Polisacaridos").
elementos(gordolobo, "Saponinas, Flavonoides").
elementos(grama, "Flavonoides, Silicatos").

produce_medicamentos(marihuana, "Si").
produce_medicamentos(mastuerzo, "No").
produce_medicamentos(matarique, "No").
produce_medicamentos(menta, "No").
produce_medicamentos(girasol, "No").
produce_medicamentos(gingseng, "Si").
produce_medicamentos(gordolobo, "No").
produce_medicamentos(grama, "No").

medicamentos_provenientes(marihuana, ["Marinol", "Cesamet"]).
medicamentos_provenientes(mastuerzo, []).
medicamentos_provenientes(matarique, []).
medicamentos_provenientes(menta, []).
medicamentos_provenientes(girasol, []).
medicamentos_provenientes(gingseng, ["Ginsana"]).
medicamentos_provenientes(gordolobo, []).
medicamentos_provenientes(grama, []).

afecciones_medicamentos(marihuana, ["Alivia dolor cronico", "Reduce nauseas", "Estimula el apetito"]).
afecciones_medicamentos(mastuerzo, []).
afecciones_medicamentos(matarique, []).
afecciones_medicamentos(menta, []).
afecciones_medicamentos(girasol, []).
afecciones_medicamentos(gingseng, ["Mejora el estado general de salud", "Aumenta energia"]).
afecciones_medicamentos(gordolobo, []).
afecciones_medicamentos(grama, []).

efectos_planta(marihuana, ["Alivio del dolor", "Relajacion", "Aumento del apetito"]).
efectos_planta(mastuerzo, ["Mejora el sistema inmunologico", "Propiedades antisepticas"]).
efectos_planta(matarique, ["Alivio de problemas respiratorios", "Reduccion de inflamacion"]).
efectos_planta(menta, ["Mejora la digestion", "Alivio del dolor de cabeza", "Refrescante"]).
efectos_planta(girasol, ["Alivio del dolor de estomago", "Reduccion del nerviosismo"]).
efectos_planta(gingseng, ["Mejora el estado general de salud", "Aumenta energia"]).
efectos_planta(gordolobo, ["Alivio de la bronquitis", "Propiedades expectorantes"]).
efectos_planta(grama, ["Reduccion de inflamaciones", "Propiedades diureticas"]).

significado_palabra("analgesico", "Sustancia que reduce o elimina el dolor.").
significado_palabra("antiseptico", "Sustancia que previene la infeccion al inhibir el crecimiento de microorganismos.").

acciones_efectos(marihuana, ["Analgesico", "Anti-inflamatorio", "Relajante"]).
acciones_efectos(mastuerzo, ["Antiseptico", "Expectorante"]).
acciones_efectos(matarique, ["Expectorante", "Anti-inflamatorio"]).
acciones_efectos(menta, ["Digestivo", "Analgesico", "Refrescante"]).
acciones_efectos(girasol, ["Calmante", "Relajante"]).
acciones_efectos(gingseng, ["Energizante", "Adaptogeno"]).
acciones_efectos(gordolobo, ["Expectorante", "Anti-inflamatorio"]).
acciones_efectos(grama, ["Diuretico", "Anti-inflamatorio"]).

es_analgesica(marihuana, "Se").
es_analgesica(mastuerzo, "No").
es_analgesica(matarique, "No").
es_analgesica(menta, "Se").
es_analgesica(girasol, "No").
es_analgesica(gingseng, "No").
es_analgesica(gordolobo, "No").
es_analgesica(grama, "No").

planta_nombre_cientifico(marihuana, "Cannabis sativa").
planta_nombre_cientifico(mastuerzo, "Tropaeolum majus").
planta_nombre_cientifico(matarique, "Calea urticifolia").
planta_nombre_cientifico(menta, "Mentha piperita").
planta_nombre_cientifico(girasol, "Helianthus annuus").
planta_nombre_cientifico(gingseng, "Panax ginseng").
planta_nombre_cientifico(gordolobo, "Verbascum thapsus").
planta_nombre_cientifico(grama, "Cynodon dactylon").

enfermedades_curan_planta(marihuana, ["Dolor cronico", "Nauseas", "Perdida de apetito"]).
enfermedades_curan_planta(mastuerzo, ["Bronquitis", "Infecciones de las vias respiratorias"]).
enfermedades_curan_planta(matarique, ["Problemas respiratorios", "Inflamacion"]).
enfermedades_curan_planta(menta, ["Indigestion", "Dolor de cabeza"]).
enfermedades_curan_planta(girasol, ["Dolor de estomago", "Nerviosismo"]).
enfermedades_curan_planta(gingseng, ["Fatiga", "Estres"]).
enfermedades_curan_planta(gordolobo, ["Bronquitis"]).
enfermedades_curan_planta(grama, ["Inflamaciones"]).

cura_enfermedad(marihuana, "Dolor cronico", "Si").
cura_enfermedad(mastuerzo, "Bronquitis", "Si").
cura_enfermedad(matarique, "Problemas respiratorios", "Si").
cura_enfermedad(menta, "Indigestion", "Si").
cura_enfermedad(girasol, "Dolor de estomago", "Si").
cura_enfermedad(gingseng, "Fatiga", "Si").
cura_enfermedad(gordolobo, "Bronquitis", "Si").
cura_enfermedad(grama, "Inflamaciones", "Si").

formas_preparacion_tratamiento(marihuana, general, "Fumar, Vaporizar, Ingesta").
formas_preparacion_tratamiento(marihuana, especifico, "Fumar para alivio rapido del dolor, Vaporizar para evitar irritacion pulmonar").
formas_preparacion_tratamiento(mastuerzo, general, "Infusion, Consumo directo").
formas_preparacion_tratamiento(matarique, general, "Infusion, Decoccion").
formas_preparacion_tratamiento(menta, general, "Infusion, Uso topico").
formas_preparacion_tratamiento(girasol, general, "Infusion, Decoccion").
formas_preparacion_tratamiento(gingseng, general, "Infusion, Extracto").
formas_preparacion_tratamiento(gordolobo, general, "Infusion, Decoccion").
formas_preparacion_tratamiento(grama, general, "Infusion, Decoccion").

tratamiento_preparacion(marihuana, dolor_cronico, "Fumar marihuana para alivio inmediato").
tratamiento_preparacion(mastuerzo, bronquitis, "Preparar una infusion con hojas de mastuerzo y beber dos veces al dia").
tratamiento_preparacion(matarique, problemas_respiratorios, "Hacer una decoccion de matarique y beber una taza al dia").
tratamiento_preparacion(menta, indigestión, "Preparar una infusion de menta y beber despues de las comidas").
tratamiento_preparacion(girasol, dolor_estomago, "Preparar una infusion de girasol y beber despues de las comidas").
tratamiento_preparacion(gordolobo, bronquitis, "Preparar una infusion de gordolobo y beber dos veces al dia").
tratamiento_preparacion(gingseng, fatiga, "Tomar extracto de gingseng en la mañana").
tratamiento_preparacion(grama, inflamaciones, "Preparar una infusion de grama y beber dos veces al dia").

origen_planta(marihuana, "America").
origen_planta(mastuerzo, "Europa").
origen_planta(matarique, "America").
origen_planta(menta, "Europa").
origen_planta(girasol, "America").
origen_planta(gingseng, "Asia").
origen_planta(gordolobo, "Europa").
origen_planta(grama, "Africa").

tratamiento_enfermedad(dolor_cronico, ["Fumar marihuana", "Tomar Marinol"]).
tratamiento_enfermedad(bronquitis, ["Infusion de mastuerzo", "Infusion de gordolobo"]).
tratamiento_enfermedad(problemas_respiratorios, ["Decoccion de matarique"]).
tratamiento_enfermedad(indigestión, ["Infusion de menta"]).
tratamiento_enfermedad(dolor_estomago, ["Infusion de girasol"]).
tratamiento_enfermedad(fatiga, ["Extracto de gingseng"]).
tratamiento_enfermedad(inflamaciones, ["Infusion de grama"]).

botiquin_plantas(["Marihuana", "Menta", "Matarique", "Mastuerzo", "Girasol", "Gingseng", "Gordolobo", "Grama"]).

% Predicado para obtener información específica según la pregunta y el tipo de planta
obtener_respuesta('¿Es planta medicinal? (general y especifico)', Tipo, Respuesta) :-
    es_planta_medicinal(Tipo, general, General),
    es_planta_medicinal(Tipo, especifico, Especifico),
    format(atom(Respuesta), 'General: ~w\nEspecifico: ~w', [General, Especifico]).

obtener_respuesta('¿Que elementos tiene?', Tipo, Respuesta) :-
    elementos(Tipo, Elementos),
    format(atom(Respuesta), 'Elementos: ~w', [Elementos]).

obtener_respuesta('¿Es una planta que produce medicamentos? Si si, ¿cuales?', Tipo, Respuesta) :-
    produce_medicamentos(Tipo, Produce),
    medicamentos_provenientes(Tipo, Medicamentos),
    format(atom(Respuesta), 'Produce medicamentos: ~w\nMedicamentos: ~w', [Produce, Medicamentos]).

obtener_respuesta('¿Cuales son los medicamentos provenientes de la planta?', Tipo, Respuesta) :-
    medicamentos_provenientes(Tipo, Medicamentos),
    format(atom(Respuesta), 'Medicamentos: ~w', [Medicamentos]).

obtener_respuesta('¿Cuales son las afecciones o efectos de medicamentos provenientes de la planta?', Tipo, Respuesta) :-
    afecciones_medicamentos(Tipo, Afecciones),
    format(atom(Respuesta), 'Afecciones o efectos: ~w', [Afecciones]).

obtener_respuesta('¿Cuales son los efectos de la planta?', Tipo, Respuesta) :-
    efectos_planta(Tipo, Efectos),
    format(atom(Respuesta), 'Efectos: ~w', [Efectos]).

obtener_respuesta('Significado de palabras que son acciones o efectos de plantas sobre el organismo.', _, Respuesta) :-
    findall(Sig, significado_palabra(_, Sig), Significados),
    format(atom(Respuesta), 'Significados: ~w', [Significados]).

obtener_respuesta('Lista de la planta con sus acciones o efectos sobre el organismo.', Tipo, Respuesta) :-
    acciones_efectos(Tipo, Acciones),
    format(atom(Respuesta), 'Acciones o efectos: ~w', [Acciones]).

obtener_respuesta('¿Es analgesica?', Tipo, Respuesta) :-
    es_analgesica(Tipo, EsAnalgesica),
    format(atom(Respuesta), 'Es analgesica: ~w', [EsAnalgesica]).

obtener_respuesta('Lista de plantas medicinales con su nombre cientifico', _, Respuesta) :-
    findall(Nombre, planta_nombre_cientifico(_, Nombre), NombresCientificos),
    format(atom(Respuesta), 'Plantas medicinales con su nombre cientifico: ~w', [NombresCientificos]).

obtener_respuesta('¿Cuales son las enfermedades que curan las plantas?', Tipo, Respuesta) :-
    enfermedades_curan_planta(Tipo, Enfermedades),
    format(atom(Respuesta), 'Enfermedades que curan: ~w', [Enfermedades]).

obtener_respuesta('¿Cual es la enfermedad que cura una planta?', Tipo, Respuesta) :-
    findall([Enfermedad, SiNo], cura_enfermedad(Tipo, Enfermedad, SiNo), Enfermedades),
    format(atom(Respuesta), 'Enfermedades que cura: ~w', [Enfermedades]).

obtener_respuesta('¿Cuales son las formas de preparacion para tratamiento de enfermedades con plantas? (general y especifico)', Tipo, Respuesta) :-
    formas_preparacion_tratamiento(Tipo, general, General),
    formas_preparacion_tratamiento(Tipo, especifico, Especifico),
    format(atom(Respuesta), 'Formas de preparacion\nGeneral: ~w\nEspecifico: ~w', [General, Especifico]).

obtener_respuesta('¿Cual es el tratamiento y preparacion para alguna enfermedad?', Tipo, Respuesta) :-
    findall([Enfermedad, Tratamiento], tratamiento_preparacion(Tipo, Enfermedad, Tratamiento), Tratamientos),
    format(atom(Respuesta), 'Tratamientos y preparaciones: ~w', [Tratamientos]).

obtener_respuesta('Origenes de las plantas medicinales', Tipo, Respuesta) :-
    origen_planta(Tipo, Origen),
    format(atom(Respuesta), 'Origen: ~w', [Origen]).

obtener_respuesta('Origen de una planta.', Tipo, Respuesta) :-
    origen_planta(Tipo, Origen),
    format(atom(Respuesta), 'Origen: ~w', [Origen]).

obtener_respuesta('¿Cual es el tratamiento para alguna enfermedad ya sea con plantas o medicamentos?', Tipo, Respuesta) :-
    findall([Enfermedad, Tratamiento], tratamiento_enfermedad(Enfermedad, Tratamiento), Tratamientos),
    format(atom(Respuesta), 'Tratamientos para enfermedades: ~w', [Tratamientos]).

obtener_respuesta('Botiquin de plantas.', _, Respuesta) :-
    botiquin_plantas(Plantas),
    format(atom(Respuesta), 'Botiquin de plantas: ~w', [Plantas]).

% Función para mostrar la respuesta en una ventana
mostrar_respuesta(Pregunta, Tipo) :-
    % Obtener la respuesta específica según la pregunta y el tipo de planta
    obtener_respuesta(Pregunta, Tipo, Respuesta),
    % Crear una nueva ventana con la respuesta
    new(VentanaRespuesta, dialog('Respuesta')),
    new(LabelRespuesta, label(texto, Respuesta)),
    send(VentanaRespuesta, append, LabelRespuesta),
    send(VentanaRespuesta, open).

% Crear botones para cada pregunta
crear_botones([], _, _, _, _).
crear_botones([Pregunta | Resto], Ventana, PosX, PosY, Tipo) :-
    new(Boton, button(Pregunta)), % Crear botón con la pregunta
    send(Boton, message, message(@prolog, mostrar_respuesta, Pregunta, Tipo)), % Asignar acción al botón
    send(Ventana, display, Boton, point(PosX, PosY)), % Mostrar el botón
    PosY1 is PosY + 40,
    crear_botones(Resto, Ventana, PosX, PosY1, Tipo).

% Botones para cada pregunta
preguntas_botones(Ventana, Tipo) :-
    Preguntas = [
        '¿Es planta medicinal? (general y especifico)',
        '¿Que elementos tiene?',
        '¿Es una planta que produce medicamentos? Si si, ¿cuales?',
        '¿Cuales son los medicamentos provenientes de la planta?',
        '¿Cuales son las afecciones o efectos de medicamentos provenientes de la planta?',
        '¿Cuales son los efectos de la planta?',
        'Significado de palabras que son acciones o efectos de plantas sobre el organismo.',
        'Lista de la planta con sus acciones o efectos sobre el organismo.',
        '¿Es analgesica?',
        'Lista de plantas medicinales con su nombre cientifico',
        '¿Cuales son las enfermedades que curan las plantas?',
        '¿Cual es la enfermedad que cura una planta?',
        '¿Cuales son las formas de preparacion para tratamiento de enfermedades con plantas? (general y especifico)',
        '¿Cual es el tratamiento y preparacion para alguna enfermedad?',
        'Origenes de las plantas medicinales',
        'Origen de una planta.',
        '¿Cual es el tratamiento para alguna enfermedad ya sea con plantas o medicamentos?',
        'Botiquin de plantas.'
    ],
    crear_botones(Preguntas, Ventana, 30, 100, Tipo).

% Mostrar botones de las plantas para seleccionar
mostrar_botones_plantas :-
    new(VentanaPlantas, dialog('Seleccionar planta')),
    new(BotonMarihuana, button('Marihuana')),
    new(BotonMastuerzo, button('Mastuerzo')),
    new(BotonMatarique, button('Matarique')),
    new(BotonMenta, button('Menta')),
    new(BotonGirasol, button('Girasol')),
    new(BotonGingseng, button('Gingseng')),
    new(BotonGordolobo, button('Gordolobo')),
    new(BotonGrama, button('Grama')),
    send(BotonMarihuana, message, message(@prolog, iniciar_con_planta, marihuana)),
    send(BotonMastuerzo, message, message(@prolog, iniciar_con_planta, mastuerzo)),
    send(BotonMatarique, message, message(@prolog, iniciar_con_planta, matarique)),
    send(BotonMenta, message, message(@prolog, iniciar_con_planta, menta)),
    send(BotonGirasol, message, message(@prolog, iniciar_con_planta, girasol)),
    send(BotonGingseng, message, message(@prolog, iniciar_con_planta, gingseng)),
    send(BotonGordolobo, message, message(@prolog, iniciar_con_planta, gordolobo)),
    send(BotonGrama, message, message(@prolog, iniciar_con_planta, grama)),
    send(VentanaPlantas, append, BotonMarihuana),
    send(VentanaPlantas, append, BotonMastuerzo),
    send(VentanaPlantas, append, BotonMatarique),
    send(VentanaPlantas, append, BotonMenta),
    send(VentanaPlantas, append, BotonGirasol),
    send(VentanaPlantas, append, BotonGingseng),
    send(VentanaPlantas, append, BotonGordolobo),
    send(VentanaPlantas, append, BotonGrama),
    send(VentanaPlantas, open).

% Iniciar con una planta seleccionada
iniciar_con_planta(Tipo) :-
    new(Ventana, dialog('Preguntas sobre plantas medicinales')),
    preguntas_botones(Ventana, Tipo),
    send(Ventana, open).

% Iniciar la aplicación
inicio :-
    mostrar_botones_plantas.

% Ejecutar el inicio
:- initialization(inicio).
