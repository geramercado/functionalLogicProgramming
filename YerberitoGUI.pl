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



% Base de datos de plantas y sus propiedades
planta_propiedad(marihuana, analgesica).
planta_propiedad(marihuana, antiinflamatoria).
planta_propiedad(marihuana, relajante).
planta_propiedad(mastuerzo, expectorante).
planta_propiedad(mastuerzo, digestiva).
planta_propiedad(matarique, cicatrizante).
planta_propiedad(matarique, antibacteriana).
planta_propiedad(menta, digestiva).
planta_propiedad(menta, refrescante).
planta_propiedad(girasol, antiinflamatoria).
planta_propiedad(girasol, nutritiva).
planta_propiedad(gingseng, energetica).
planta_propiedad(gingseng, antioxidante).
planta_propiedad(gordolobo, expectorante).
planta_propiedad(gordolobo, antiinflamatoria).
planta_propiedad(grama, diuretica).
planta_propiedad(grama, depurativa).
planta_propiedad(aloevera, hidratante).
planta_propiedad(aloevera, cicatrizante).
planta_propiedad(aloevera, antiinflamatoria).
planta_propiedad(manazanilla, digestiva).
planta_propiedad(manazanilla, calmante).
planta_propiedad(lavanda, relajante).
planta_propiedad(lavanda, antiseptica).
planta_propiedad(romero, estimulante).
planta_propiedad(romero, digestiva).
planta_propiedad(tomillo, antiseptico).
planta_propiedad(tomillo, expectorante).
planta_propiedad(salvia, astringente).
planta_propiedad(salvia, antioxidante).


%------- pal botiquinn --------------

planta(malva).
planta(romero).
planta(matico).
planta(palo_de_brasil).
planta(albahaca).
planta(ruda).
planta(quiebra_piedra).

uso(malva, aliviar_problemas_gastrointestinales).
uso(malva, aliviar_tos_bronquitis).
uso(romero, estimulante_sistema_nervioso).
uso(romero, mejorar_circulacion).
uso(romero, aliviar_dolores_musculares).
uso(matico, cicatrizante).
uso(matico, antiséptico).
uso(matico, tratar_heridas).
uso(matico, tratar_ulceras_bucales).
uso(palo_de_brasil, febrifugo).
uso(palo_de_brasil, tonico).
uso(palo_de_brasil, tratar_fiebre).
uso(albahaca, mejorar_digestion).
uso(albahaca, aliviar_dolores_de_cabeza).
uso(albahaca, reducir_inflamacion).
uso(ruda, estimulante_sistema_digestivo).
uso(ruda, emenagoga).
uso(ruda, aliviar_dolores_menstruales).
uso(ruda, repelente_de_insectos).
uso(quiebra_piedra, tratar_calculos_renales).
uso(quiebra_piedra, tratar_infecciones_urinarias).
uso(quiebra_piedra, tratar_hepatitis).
uso(quiebra_piedra, tratar_diabetes).


usos_de_planta(Planta, Usos) :-
    findall(Uso, uso(Planta, Uso), Usos).

botiquin_plantas(["Marihuana", "Menta", "Matarique", "Mastuerzo", "Girasol", "Gingseng", "Gordolobo", "Grama"]).


%% Hechos sobre las formas de preparar una planta según el Yerberito Ilustrado %%%%%%%%%%%%%%%%%%

% Cocimiento
forma_preparacion(cocimiento, 'Cocimiento es una tecnica de extraccion de principios activos mediante ebullicion en agua. Se emplea para afecciones internas.').
uso_preparacion(cocimiento, 'Se utiliza para problemas digestivos, respiratorios, entre otros.').
procedimiento(cocimiento, 'Se hierve la planta en agua durante un tiempo determinado, luego se filtra y se consume el liquido resultante.').

% Infusión
forma_preparacion(infusion, 'Infusion implica verter agua caliente sobre partes de la planta para disolver compuestos.').
uso_preparacion(infusion, 'Se utiliza para aliviar dolencias menores como insomnio, ansiedad o problemas digestivos.').
procedimiento(infusion, 'Se vierte agua caliente sobre las partes de la planta y se deja reposar durante un tiempo determinado.').

% Maceración
forma_preparacion(maceracion, 'Maceracion consiste en dejar reposar partes de la planta en un liquido para extraer compuestos activos.').
uso_preparacion(maceracion, 'Se utiliza para extraer componentes liposolubles o hidrosolubles de las plantas para preparar tinturas, aceites medicinales, entre otros.').
procedimiento(maceracion, 'Se deja reposar partes de la planta en alcohol, aceite o agua durante un periodo de tiempo determinado.').

% Jarabe
forma_preparacion(jarabe, 'Jarabe combina extractos de plantas con azucar o miel para hacer remedios herbales mas apetecibles.').
uso_preparacion(jarabe, 'Se utiliza para aliviar la tos, mejorar la congestin nasal y otras dolencias, especialmente en niños o personas con dificultades para tomar preparaciones amargas.').
procedimiento(jarabe, 'Se combinan extractos de plantas con azucar o miel para obtener un líquido viscoso y dulce.').

% Tintura
forma_preparacion(tintura, 'Tintura es una preparacion donde los principios activos de la planta se extraen y concentran en alcohol o vinagre.').
uso_preparacion(tintura, 'Se utiliza para conservar las propiedades medicinales de las plantas y facilitar su dosificacion durante largos periodos de tiempo.').
procedimiento(tintura, 'Los principios activos de la planta se extraen y concentran en alcohol o vinagre.').

% Jugos
forma_preparacion(jugo, 'Jugo es la extraccion de los liquidos de la planta, generalmente mediante exprimido o trituracion.').
uso_preparacion(jugo, 'Se utiliza para consumir los nutrientes y beneficios de la planta de forma liquida, rica en vitaminas y minerales.').
procedimiento(jugo, 'Se extraen los líquidos de la planta mediante exprimido o trituración.').

% Horchata
forma_preparacion(horchata, 'Horchata es una bebida refrescante preparada a partir de semillas o frutos secos, como la chufa o el arroz.').
uso_preparacion(horchata, 'Se utiliza como bebida refrescante y a veces como remedio digestivo.').
procedimiento(horchata, 'Se remojan las semillas o frutos secos en agua, se mezclan y se filtran para obtener una bebida refrescante.').


% ----Formas de preparación para las nuevas plantas ............ agregar mas, para poder preguntar cuales plantas se hacen de alguna manera

planta(marihuana).
planta(menta).
planta(matarique).
planta(mastuerzo).
planta(girasol).
planta(ginseng).
planta(gordolobo).
planta(grama).

% Marihuana
forma_preparacion(marihuana, cocimiento).
uso_preparacion(marihuana, 'Se utiliza para problemas digestivos, respiratorios, entre otros.').
procedimiento(marihuana, 'Se hierve la planta en agua durante un tiempo determinado, luego se filtra y se consume el liquido resultante.').

% Menta
forma_preparacion(menta, infusion).
uso_preparacion(menta, 'Se utiliza para aliviar dolencias menores como insomnio, ansiedad o problemas digestivos.').
procedimiento(menta, 'Se vierte agua caliente sobre las partes de la planta y se deja reposar durante un tiempo determinado.').

% Matarique
forma_preparacion(matarique, cocimiento).
uso_preparacion(matarique, 'Se utiliza para tratar problemas digestivos y como calmante estomacal.').
procedimiento(matarique, 'Se hierve la planta en agua durante un tiempo determinado, luego se filtra y se consume el liquido resultante.').

% Mastuerzo
forma_preparacion(mastuerzo, infusion).
uso_preparacion(mastuerzo, 'Se utiliza como expectorante y para aliviar dolores de garganta.').
procedimiento(mastuerzo, 'Se vierte agua caliente sobre las partes de la planta y se deja reposar durante un tiempo determinado.').

% Girasol
forma_preparacion(girasol, infusion).
uso_preparacion(girasol, 'Se utiliza como antiinflamatorio y para aliviar dolores musculares.').
procedimiento(girasol, 'Se vierte agua caliente sobre las partes de la planta y se deja reposar durante un tiempo determinado.').

% Gingseng
forma_preparacion(ginseng, tintura).
uso_preparacion(ginseng, 'Se utiliza como estimulante y para mejorar la resistencia fisica y mental.').
procedimiento(ginseng, 'Los principios activos de la planta se extraen y concentran en alcohol o vinagre.').

% Gordolobo
forma_preparacion(gordolobo, infusion).
uso_preparacion(gordolobo, 'Se utiliza como expectorante y para aliviar la tos y los problemas respiratorios.').
procedimiento(gordolobo, 'Se vierte agua caliente sobre las partes de la planta y se deja reposar durante un tiempo determinado.').

% Grama
forma_preparacion(grama, jugo).
uso_preparacion(grama, 'Se utiliza como diuretico y para eliminar toxinas del organismo.').
procedimiento(grama, 'Se extraen los liquidos de la planta mediante exprimido o trituracion.').


% Predicados para acceder a la información
forma_uso_preparacion(Forma, Uso) :-
    forma_preparacion(Forma, _),
    uso_preparacion(Forma, Uso).

procedimiento_preparacion(Forma, Procedimiento) :-
    forma_preparacion(Forma, _),
    procedimiento(Forma, Procedimiento).




%% Predicados para acceder a la información

% Forma de uso y preparación de una planta
forma_uso_preparacion(Planta, Forma, Uso) :-
    forma_preparacion(Planta, Forma),
    uso_preparacion(Planta, Uso).

% Procedimiento de preparación de una planta
procedimiento_preparacion(Planta, Procedimiento) :-
    forma_preparacion(Planta, _),
    procedimiento(Planta, Procedimiento).

% Despliega todas las plantas que se preparan de una forma específica
plantas_por_forma(Forma, Plantas) :-
    findall(Planta, forma_preparacion(Planta, Forma), Plantas).


% ------------------- para obtener información específica según la pregunta y el tipo de planta (ventana de preguntas de alguna planta)--------------------
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


%%%%%%%%% .--------------------------------------. %%%%%%%%%%%%%%%%%


% Relación entre plantas y enfermedades que curan
planta_enfermedad('Marihuana', ['Dolor crónico', 'Náuseas', 'Espasmos musculares']).
planta_enfermedad('Mastuerzo', ['Escorbuto', 'Anemia']).
planta_enfermedad('Matarique', ['Reumatismo', 'Artritis']).
planta_enfermedad('Menta', ['Indigestión', 'Dolor de cabeza']).
planta_enfermedad('Girasol', ['Bronquitis', 'Asma']).
planta_enfermedad('Gingseng', ['Fatiga', 'Estrés']).
planta_enfermedad('Gordolobo', ['Tos', 'Gripe']).
planta_enfermedad('Grama', ['Cálculos renales', 'Inflamación']).

% Mostrar botones de las plantas para seleccionar
mostrar_botones_plantas :-
    new(VentanaPlantas, dialog('Seleccionar planta')),
    
    new(GrupoPlantas1, dialog_group('Grupo 1', group)),
    new(GrupoPlantas2, dialog_group('Grupo 2', group)),
    new(BotonMarihuana, button('Marihuana', message(@prolog, iniciar_con_planta, 'Marihuana'))),
    new(BotonMastuerzo, button('Mastuerzo', message(@prolog, iniciar_con_planta, 'Mastuerzo'))),
    new(BotonMatarique, button('Matarique', message(@prolog, iniciar_con_planta, 'Matarique'))),
    new(BotonMenta, button('Menta', message(@prolog, iniciar_con_planta, 'Menta'))),
    new(BotonGirasol, button('Girasol', message(@prolog, iniciar_con_planta, 'Girasol'))),
    new(BotonGingseng, button('Gingseng', message(@prolog, iniciar_con_planta, 'Gingseng'))),
    new(BotonGordolobo, button('Gordolobo', message(@prolog, iniciar_con_planta, 'Gordolobo'))),
    new(BotonGrama, button('Grama', message(@prolog, iniciar_con_planta, 'Grama'))),
    send(GrupoPlantas1, append, BotonMarihuana),
    send(GrupoPlantas1, append, BotonMastuerzo),
    send(GrupoPlantas1, append, BotonMatarique),
    send(GrupoPlantas1, append, BotonMenta),
    send(GrupoPlantas2, append, BotonGirasol),
    send(GrupoPlantas2, append, BotonGingseng),
    send(GrupoPlantas2, append, BotonGordolobo),
    send(GrupoPlantas2, append, BotonGrama),
        
    % Tercer grupo con otras opciones
    new(GrupoOpciones, dialog_group('Opciones', group)),
    new(BotonPreguntaEnfermedades, button('Pregunta enfermedades', message(@prolog, mostrar_enfermedades_planta))),
    new(BotonPropiedades, button('Propiedades', message(@prolog, mostrar_ventana_propiedades))),
    new(BotonBotiquin, button('Botiquin', message(@prolog, mostrar_botiquin))),
    new(BotonComoPreparar, button('Como preparar', message(@prolog, abrir_ventana_como_preparar))),
    new(BotonFormasPreparacion, button('Formas de preparacion', message(@prolog, mostrar_formas_preparacion))),
    new(BotonBuscarFormasPreparar, button('Buscar formas preparar', message(@prolog, buscar_formas_preparacion))),
    send(GrupoOpciones, append, BotonPreguntaEnfermedades),
    send(GrupoOpciones, append, BotonPropiedades),
    send(GrupoOpciones, append, BotonBotiquin),
    send(GrupoOpciones, append, BotonComoPreparar),
    send(GrupoOpciones, append, BotonFormasPreparacion),
    send(GrupoOpciones, append, BotonBuscarFormasPreparar),

    % Agregar grupos a la ventana principal
    send(VentanaPlantas, append, GrupoPlantas1),
    send(VentanaPlantas, append, GrupoPlantas2),
    send(VentanaPlantas, append, GrupoOpciones),
    send(VentanaPlantas, open).

% Predicado para manejar la acción del botón "Pregunta enfermedades"
mostrar_enfermedades_planta :-
    new(Dialog, dialog('Enfermedades que cura la planta')),
    send(Dialog, append, new(TextoPlanta, text_item('Nombre de la planta'))),
    send(Dialog, append, button('Buscar', message(@prolog, buscar_y_mostrar_enfermedades, TextoPlanta?selection))),
    send(Dialog, append, button('Cancelar', message(Dialog, destroy))),
    send(Dialog, open).

% Predicado para buscar y mostrar las enfermedades que cura la planta
buscar_y_mostrar_enfermedades(Planta) :-
    (   planta_enfermedad(Planta, Enfermedades) ->
        atomic_list_concat(Enfermedades, ', ', EnfermedadesStr),
        new(ResultDialog, dialog('Resultados de búsqueda')),
        send(ResultDialog, append, label(nombre, string('Enfermedades que cura %s: %s', Planta, EnfermedadesStr))),
        send(ResultDialog, append, button('OK', message(ResultDialog, destroy))),
        send(ResultDialog, open)
    ;   send(@display, inform, 'Planta no encontrada o no se tienen registradas enfermedades que cura.')
    ).

% Iniciar con una planta seleccionada
iniciar_con_planta(Planta) :-
    new(Ventana, dialog('Preguntas sobre plantas medicinales')),
    preguntas_botones(Ventana, Planta),
    send(Ventana, open).

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

% Iniciar la aplicación
inicio :-
    mostrar_botones_plantas.

% Ejecutar el inicio
:- initialization(inicio).

