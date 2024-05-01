
% Importacion de la libreria random

 :- use_module(library(random)).

% Regla principal que llama mensaje de bienvenida y procesa la
% conversaci�n
mrtrainer:-
	inicio_conversacion.

% Regla que crea un loop para procesar la conversaci�n hasta que el
% usuario se despida
inicio_conversacion:-
	repeat,
	print_prompt(you), %impresion de palabra "cliente en pantalla"
	readin(S),
	gen_respuesta(S,R),
	print_prompt(me),
	write_list(R),
	despedida(S), !, halt.

mr_trainer('MrTrainer').
cliente('Cliente').

% Regla que imprime mensaje de lista en terminal
write_list([]):- nl.
write_list([H|T]):- write(H), write(' '), write_list(T).

% Regla que llama al name del usuario chat para simular interfaz de
% chat
print_prompt(me):- %impresion de "mr trainer en pantalla"
        mr_trainer(X), write(X), write(': '), flush_output.

% Regla que llama al name del usuario que utiliza el programa para
% simular interfaz de chat
print_prompt(you):-
        cliente(X), write(X), write(': '), flush_output.

% Regla que identifica el mensaje del usuario
readin(S):- read_in(L), mi_filter(L,S).

% lee la entrada y la unifica en una lista
read_in(P):- initread(L), words(P, L, []).

% Lee el primer y segundo caracter ingresado por el usuario, llama a leer el
% resto de la lista
initread([K1,K2|U]):- get_code(K1),get_code(K2),leer_resto(K2,U).

% ---------------Reglas que procesan la lectura de caracteres-------

% ------ REVISAR --------------
% Crea atoms?
mi_filter([],[]).
mi_filter(['\n'|T], R):- !, mi_filter(T, R).
mi_filter([nb(2), X|T], [Rm|R]):-
		name(X, CharList),
        q_follonosotrosd_by_nb(CharList),!,
        name(Rm, [50 | CharList]),
        mi_filter(T, R).
mi_filter([X|T], [X|R]):-
        mi_filter(T, R).

% Si se recibe oraci�n, procesar palabras recursivamente
words([V|U]) --> word(V),!, blanks, words(U).
% Si no hay nada, regresa vac�o
words([]) --> [].

% Procesa palabra en ASCII,
word(U1) --> [K],{lc(K,K1)},!,alphanums(U2),{name(U1,[K1|U2])}.
word(nb(N)) --> [K],{digit(K)},!,digits(U),{name(N,[K|U])}.
word(V) --> [K],{name(V,[K])}.

% Genera recursivamente caracteres alfanum�ricos en base a los c�digos
% recibidos
alphanums([K1|U]) --> [K],{alphanum(K,K1)},!,alphanums(U).
alphanums([]) --> [].

% REVISAR
alphanum(95,95) :- !.
alphanum(K,K1):- lc(K,K1).
% REVISAR
alphanum(K,K):- digit(K).

% Si es un espacio o otra se�al desconocida
blanks --> [K], {K=<32},!, blanks.
% De lo contrario, blanks son vac�os
blanks --> [].

% REVISAR
q_follonosotrosd_by_nb([113, X|_]):- digit(X).

% Si el caracter es un n�mero, se retorna
digits([K|U]) --> [K],{digit(K)},!,digits(U).
% Si es vac�o se regresa lista vac�a
digits([]) --> [].

% Verifica si es un n�mero
digit(K):- K>47, K<58.

% Procesa letras en ASCII, may�sculas y min�sculas
lc(K,K1):- K>64,K<91,!,K1 is K+32.
lc(K,K):- K>96,K<123.

% Llama recursivamente la palabra para leer sus letras
leer_resto(63,[]):- !.
leer_resto(33,[]):- !.
leer_resto(10,[]):- !.
leer_resto(K,[K1|U]):- K=<32,!,get_code(K1),leer_resto(K1,U).
leer_resto(_K1,[K2|U]):- get_code(K2),leer_resto(K2,U).

% --Reglas que procesan entrada del usuario y respuesta de mrtrainer

% Respuesta si mrtrainer no entiende entrada
% gen_respuesta(S, R):-
% \+ questionamiento(S), \+ despedida(S), \+ agradecimiento(S), \+ saludando(S),
% !, respuestas_db(random_s, Res), selec_rand(Res, R).

% Respuesta si usuario dijo alg�n tipo de saludo
gen_respuesta(S, R):-
	saludando(S), !,
	respuestas_db(saludo, Res),
	selec_rand(Res, R).

% Respuesta si usuario dijo alg�n tipo de gracias
gen_respuesta(S, R):-
	agradecimiento(S), !,
	respuestas_db(gracias, Res),
	selec_rand(Res, R).

% Respuesta si usuario dijo alg�n tipo de despedida
gen_respuesta(S, R):-
	despedida(S), !,
	respuestas_db(bye, Res),
	selec_rand(Res, R).

%respuesta si usuario dijo que quiere hacer alguna rutina

%gen_respuesta(S, R):-
%	question(Tree2, S, _Rest),
%	mapping(s2name,Tree1, Tree2), !,
%	oracion(Tree1, Rep,[]),
%	append(Rep, ['!'], R).

%gen_respuesta(S, R):-
%	question(Tree2, S, _Rest), !,
%	mapping(s2how,Tree1, Tree2),
%	oracion(Tree1, Rep,[]), !,
%	append(Rep, ['!'], R).

%gen_respuesta(S, R):-
	%oracion(Tree1, S, _Rest), !,
	%mapping(s2why,Tree1, Tree2),
	%question(Tree2, Rep,[]),
	%append(Rep, ['?'], R).

%gen_respuesta(S, R):-
%	question(Tree2, S, _Rest), !,
%	mapping(s2q,Tree1, Tree2),
%	oracion(Tree1, Rep,[]),
%	append([yes, ','|Rep], ['!'], R).


	gen_respuesta(S, R):-
		oracion(Tree1, S, _Rest), !,
		mapping(inicio_ases,Tree1, Tree2),
		question(Tree2, Rep,[]),
		append(Rep, ['?'], R).




% Regla que regresa a partir de una lista el valor del �ndice
% especificado
nth_item([H|_], 1, H).
nth_item([_|T], N, X):-
	nth_item(T, N1, X),
	N is N1 + 1.

% Regla que escoge un valor aleatorio de una lista y lo retorna
selec_rand(Res, R):-
	length(Res, Length),
	Upper is Length + 1,
	random(1, Upper, Rand),
	nth_item(Res, Rand, R).

% ------------ Definiciones de Definite Clause Grammar para
% gram�tica libre de contexto ------------------------
%oracion(s(X,Y, is, Z)) --> belonging_phrase(X), abstract_noun(Y),
                            % [is],  special_noun(Z).

oracion(s(X, Y,Z,F,H))--> subject_phrase(X),verb(Y),complemento_d(Z),articulo(F),noun(H).
oracion(s(X, Y,Z,F,G,H))--> subject_phrase(X),verb(Y),complemento_d(Z),articulo(F),complemento_d(G) ,noun(H).
oracion(s(X, Y,Z,F,G,H))--> subject_phrase(X),verb(Y),complemento_d(Z),articulo(F),noun(H).
oracion(s(X, Y,Z))--> subject_phrase(X),verb(Y),complemento_d(Z).
oracion(s(X, Y,F,Z))--> subject_phrase(X),verb(Y),articulo(F),noun(Z).



%oracion(s(X, Y, Z)) --> subject_phrase(X), verb(Y), object_phrase(Z).

%oracion(s(X, Y, Z)) --> question(X), object_pronoun(Y), noun(Z).

pertenencia(perte(tuyo)) --> [tuyo].
pertenencia(perte(mi)) --> [mi].




subject_phrase(sp(X)) --> proname_suj(X).
subject_phrase(sp(X)) --> noun_phrase(X).

object_phrase(op(X,Y)) --> noun_phrase(X), adverb(Y).
object_phrase(op(X, Y)) --> object_pronoun(X), adverb(Y).

noun_phrase(np(X, Y)) --> articulo(X), noun(Y).
noun_phrase(np(Y)) --> noun(Y).

proname_suj(spn(yo)) --> [yo].
proname_suj(spn(nosotros)) --> [nosotros].
proname_suj(spn(usted)) --> [usted].
proname_suj(spn(ellos)) --> [ellos].
proname_suj(spn(el)) --> [el].
proname_suj(spn(ella)) --> [ella].
proname_suj(spn(eso)) --> [eso].
proname_suj(spn(quien)) --> [quien].
proname_suj(spn(entrenador)) --> [entrenador].
proname_suj(spn(me)) --> [me].

object_pronoun(opn(you))--> [you].
object_pronoun(opn(tuyo))--> [tuyo].
object_pronoun(opn(me))--> [me].
object_pronoun(opn(us))--> [us].
object_pronoun(opn(them))--> [them].
object_pronoun(opn(him))--> [him].
object_pronoun(opn(her))--> [her].
object_pronoun(opn(it))--> [it].

articulo(art([])) --> [].
articulo(art([de])) --> [de].
articulo(art([a])) --> [a].
articulo(art([mi])) --> [mi].
articulo(art([mis])) --> [mis].
articulo(art([un])) --> [un].
articulo(art([una])) --> [una].
articulo(art([el])) --> [el].
articulo(art([del])) --> [del].
articulo(art([la])) --> [la].
articulo(art([las])) --> [las].




noun(noun(ejercicio)) --> [ejercicio].
noun(noun(padecimiento)) --> [padecimiento].
noun(noun(dolor)) --> [dolor].
noun(noun(molestia)) --> [molestia].
noun(noun(problema)) --> [problema].

noun(noun(natacion)) --> [natacion].
noun(noun(ciclismo)) --> [ciclismo].
noun(noun(fondo)) --> [fondo].




noun(noun(rodilla)) --> [rodilla].
noun(noun(rodillas)) --> [rodillas].
noun(noun(articulaciones)) --> [articulaciones].
noun(noun(articulacion)) --> [articulacion].
noun(noun(tobillos)) --> [tobillos].
noun(noun(tobillo)) --> [tobillo].
noun(noun(muñecas)) --> [muñecas].







%Opciones de entrada de complemento directo
complemento_d(c_d(ganas)) --> [ganas].
complemento_d(c_d(empezar)) --> [empezar].
complemento_d(c_d(comenzar)) --> [comenzar].
complemento_d(c_d(aprender)) --> [aprender].
complemento_d(c_d(motivado)) --> [motivado].
complemento_d(c_d(comenzar)) --> [comenzar].
complemento_d(c_d(arrancar)) --> [arrancar].
complemento_d(c_d(hacer)) --> [hacer].
complemento_d(c_d(correr)) --> [correr].
complemento_d(c_d(correr)) --> [correr].









adverb(ad([very, much])) --> [very, much].
adverb(ad([how])) --> [how].
adverb(ad([])) --> [].

%Definicion de verbos  
verb(vb(gusta)) --> [gusta].
verb(vb(amo)) --> [amo].
verb(vb(es)) --> [es].
verb(vb(tengo))-->[tengo].
verb(vb(quiero))-->[quiero].
verb(vb(soy))-->[soy].

verb(vb(deseo))-->[deseo].
verb(vb(necesito))-->[necesito].
verb(vb(bajar))-->[bajar].
verb(vb(subir))-->[subir].

verb(vb(nado))-->[nado].
verb(vb(corro))-->[corro].
verb(vb(troto))-->[troto].
verb(vb(camino))-->[camino].
verb(vb(estiro))-->[estiro].

verb(vb(padezco))-->[padezco].
verb(vb(sufro))-->[sufro].
verb(vb(duele))-->[duele].
verb(vb(duelen))-->[duelen].
verb(vb(experimento))-->[experimento].




indicative_verb(ivb(are)) --> [are].
indicative_verb(ivb(am)) --> [am].

adjetivo(adj(great)) --> [great].
adjetivo(adj(good)) --> [good].
adjetivo(adj(fine)) --> [fine].



question(q(X,Y,Z)) --> [excelente, iniciativa,iniciemos].


%mapping(s2why,
 %       s(sp(spn(N1)),vb(V),op(opn(N2),ad(X))),
  %      q(why,do,s(sp(spn(P1)),vb(V),op(opn(P2),ad(X))))
   %     ) :-
    %    mapping_spn(N1, P1), mapping_opn(N2, P2).



mapping(inicio_ases,
		s(sp(spn(X)),vb(Y1),c_d(Z),art(F),c_d(G),noun(H)),	
		q(excelente,iniciativa,iniciemos)
		).

mapping(inicio_ases,
		s(sp(spn(X)),vb(Y1),c_d(H)),	
		q(excelente,iniciativa,iniciemos)
		).

mapping(inicio_ases,
			s(sp(spn(X)),vb(Y1),art(F),noun(H)),	
			q(excelente,iniciativa,iniciemos)
			).


mapping_belong(mi,tuyo).
mapping_belong(tuyo,mi).

mapping_noun(name, frank).
mapping_noun(frank, name).

mapping_indicative(are, am).
mapping_indicative(am, are).

mapping_spn(i, you).
mapping_spn(you, i).

mapping_opn(you,me).
mapping_opn(me,you).

%busca palabras clave
intersect([], _, []).
intersect([H|T1], L2, [H|T3]):-
        member(H, L2), !,
        intersect(T1, L2, T3).
intersect([_|T1], L2, L3):-
        intersect(T1, L2, L3).

% ----------- Reglas para manejar condiciones de mensaje ---------

% Regla que verifica si se mand� mensaje de despedida en la oraci�n
despedida(S):-
	despedida_db(D),
	intersect(S, D, A),
	A \== [].

% Regla que verifica si se mand� mensaje de gracias en la oraci�n
agradecimiento(S):-
	gracias_db(D),
	intersect(S, D, A),
	A \== [].

% Regla que verifica si hay signo de pregunta en la oraci�n
questionamiento(S):-
	member('?', S).

% Regla que verifica si se mand� mensaje de saludo en la oraci�n
saludando(S):-
	saludo_db(D),
	intersect(S, D, A),
	A \== [].

% -------- Bases de datos de respuestas y palabras clave ----------
% Posibles palabras clave de saludo
saludo_db([
	hola,
	buenas,
	buenos,
	saludos
	]).

% Posibles palabras clave de despedida
despedida_db([
	adios,
	listo,
	estariamos,
	chao,
	solamente,
	luego
	]).

% Posibles palabras clave de gracias
gracias_db([
	gracias,
	agradecido,
	agradecida
	]).

% Posibles mensajes de saludo de mrtrainer
respuestas_db(saludo, [
	['Cuenteme En que lo puedo ayudar?'],
	['Hola, listo para ayudar'],
	['Buenas, como esta?'],
	['Hola soy Mr. Trainer, a su servicio']
	]).

% Posibles respuestas a gracias de mrtrainer
respuestas_db(gracias, [
	['Con mucho gusto'],
	['Con gusto! Cualquier otra consulta, estoy a tu servicio'],
	['El placer es mio']
	]).

% Posibles mensajes de despedida de mrtrainer
respuestas_db(bye, [
	['Espero que haya sido de ayuda, suerte!'],
	['Que tengas buen dia'],
	['Hasta luego!'],
	['Hasta pronto!']
	]).



% Oraciones conectoras
respuestas_db(random_s, [
	['Disculpa, no entendi lo que dices'],
	['�Podrias repetir eso?'],
	['No estoy seguro de entender eso']
	]).


%rutinas principiante atletismo
respuestas_db(random_pa,[
	['Estirar por unos 5 minutos las piernas antes de iniciar. Luego dar 2 vueltas a la pista caminando a alta velocidad
    para calentar y dar zancadas por unos 100m. Para la rutina, corre por una distancia de 400 metros, empezando a un 25% de su capacidad para correr en los primeros 100
    metros y aumentando en 25% cada 100 metros, luego descansar por unos 30 segundos y repetir esta rutina unas 10 veces. Al terminar cada vuelta, no frene de golpe, mantengase
    caminando lentamente para evitar la acumulacion de acido lactico en las piernas.'],
	['Estirar por unos 5 minutos las piernas antes de iniciar. Para calentar realice un trote lento por 500m, donde cada 100m
    cambie de ejercicio entre trotar, trotar llevando las rodillas bien arriba, trotar llevando los pies hasta los gluteos, trotar dando zancadas y en los ultimos 100 metros correr hacer
    un sprint. Para la rutina, corre por una distancia de 400m a velocidad normal y al finalizarlos, realiza un sprint a toda la velocidad posible, por 100 metros.
    Disminuye la velocidad a un paso moderado de nuevo por 400metros y repite unas 5 veces el ejercicio. Al terminar camina por 100 metros disminuyendo la velocidad y
    recuerda estirar.']
])

%rutinas principiante crossfit
respuestas_db(random_pc, [
	['Estirar los brazos y piernas por unos 5 minutos y trotar unos 500m para calentar. Luego para calentar realizar dominadas negativas, brincar a la barra y descender
    lentamente, 5 series de 3 repeticiones (5x3), luego en un tabta de 20 segundos activos y 10 de descanso, realizar cada ventana activa plancha y escaladores, repitiendo
    un total de 12 veces (6 minutos). Para la rutina, seran la mayor cantidad de repeticiones posibles (AMRAP) en 20 minutos, donde una ronda consiste en 10 dominadas, 10 lagartijas
    10 toques a la barra con los pies y 200 metros de corrida. Al terminar, recuerda estirar y estar hidratandote bien.'],
	['Estirar los brazos y piernas por unos 5 minutos y hacer saltos sencillos por 10 minutos para calentar. Luego, realizaras el calentamiento llamado "burgener" con el ejercicio de clean,
    haras 5 repeticiones, iniciando desde la cadera, empujones hacia arriba, luego 5 jalones hasta el pecho, y luego 5 cleans completos que consiste en la combinacion de los movimientos
    anteriores incluyendo una rotacion de mu;ecas para descansar la barra en los hombros. Luego, realizar 5 repeticiones de 10 dominadas y 10 subidas caminando al cajon en la altura mas baja.
    Para la rutina, realiza 5 rondas de 10 saltos al cajon, 5 clean con un peso del 45% de su capacidad o unicamente la barra de 45lb y 5 dominadas. Tienes 20 minutos para completar las 5 rondas, al finalizar, recuerda estirar e hidratarte bien.']
])

%rutinas principiante halterofilia
respuestas_db(random_ph,[
	['Para calentar iniciaremos con solo la barra haciendo 5 jalones, 5 jalones al pecho y 5 cleans que seria combinando los movimientos anteriores y llevando la barra hasta los hombros, realiza esto 15 veces. Luego con solo la barra 
    realiza 20 cleans pausados, uno por uno, para familiarizarte con el ejercicio, siempre manteniendo la barra bien pegada al cuerpo. Luego, realiza 10 rondas de 5 lagartijas 10 abdominales y 15 sentadillas, para in construyendo fuerza.
    Y para finalizar, con un disco de 10kg a cada lado, realiza 30 repeticiones de clean, uno por uno, a un paso moderado, soltando la barra cada vez.'],
	['Para calentar iniciaremos con la barra, haciendo 5 jalones, 5 jalones al pecho y 5 jalones de arranque, siempre para el arranque, agarra la barra bien ancho, hasta el punto donde al subir las piernas, la barra no se levante.
    Luego realiza 10 rondas de 5 lagartijas, 10 dominadas y 15 sentadillas. Seguidamente, al finalizar estas, solo con la barra realiza 20 arranques, uno por uno, lentos
    para familiarizarte con el ejercicio. Finalmente, carga 5kg a cada lado y realiza 25 arranques, uno por uno, con la barra bien pegada al cuerpo y de forma fluida.']
])


%rutinas nivel medio atletismo
respuestas_db(random_ma,[
	['Estirar por unos 5 minutos las piernas antes de iniciar. Luegp para calentar, trotar por 400m incrementando la velocidad un 25% cada 100m 
    Al llegar a los 400m, mantener el ritmo por 800m y luego por 100m disminuir la velocidad hasta detenerse. Para la rutina hacer series de 500 metros
    almenos unas 10 series. Al terminar, recuerda estirar y bajar la velocidad gradualmente para evitar lesiones.']
])


%rutinas nivel medio crossfit
respuestas_db(random_mc,[
	['Estirar los brazos y piernas por unos 5 minutos y trotar unos 800m para calentar. Primero calentaras con 5 rondas de burgener para arranque, agarrando la barra con agarre abierto e iniciando
    desde la altura de la cadera, realiza 3 jalones, 3 jalones hasta el pecho y 3 arranques completos que consiste en la combinacion de los movimientos anteriores, siempre manteniendo la
    barra bien pegada al cuerpo, hasta llevarla a la altura arriba de la cabeza. Luego realiza por 10 minutos la mayor cantidad de rondas posibles de 5 dominadas hasta el pecho, 5 toques a la barra con los pies
    y 30 segundos en plancha. Para la rutina, realizaras tenras 20 minutos para completar 10 repeticiones de 5 arranques a un 60% de su capacidad de peso, 15 toques de los pies a la barra y 10 saltos al cajon en la altura intermedia.
    Al finalizar, recuerda estirar completamente e hidratarte bien.'],
	['Estirar los brazos y piernas por unos 5 minutos y hacer saltos dobles por 5 minutos para calentar. Para calentar realiza 5 rondas de 3 repeticiones de peso muerto aumentando el peso en
    5% cada ronda, inciando en un 50% del peso maximo. Luego realiza 5 rondas de 10 sentadillas y 10 burpees. Para la rutina, realizaras repeticiones de 21, 15 y 9, de peso muerto y burpees brincando
    por encima de la barra, tienes 15 minutos para completar la rutina o hasta donde sea posible. Al finalizar, recuerda estirar todo el cuerpo e 
    hidratarte bien.']
])

%rutinas nivel medio halterofilia
respuestas_db(random_mh,[
	['Para calentar iniciaremos con 30 repeticiones de arranque usando solo la barra. Una vez hecho esto, completa 20 repeticiones de peso muerto con 20kg a cada lado, de forma moderada, sin apuro. Una vez terminado,
    descansa por unos 5 minutos, caminando sin sentarte y luego carga un 50% del peso maximo de arranque y realiza 30 repeticiones a un paso moderado. Recuerda respirar y al finalizar estira bien el cuerpo.'],
	['Para calentar inicia trotando unos 500m, luego con la barra, realiza 10 rondas de burgener para clean para ir calentando el movimiento. Luego realiza 10 rondas de 5 repeticiones de 
    levantamientos arriba de la cabeza, lentamente para ir calentando el movimiento. Por ultimo, realiza 40 repeticiones de levantamientos clean y jerk con un peso del 50% de tu RM en el movimiento de jerk.']
])

%rutinas nivel experto atletismo
respuestas_db(random_ea,[
	['Estirar por unos 5 minutos las piernas antes de iniciar. Luego para calentar da 2 vueltas trotando a una pista o por 1600m para calentar.
    Para la rutina realizar sprints de 100m con un descanso activo de 200m hasta completar 9km (un total de 30 veces). En la ultima ronda realizala
    al 100% de la velocidad posible y en los ultimo 200m disminuir la velocidad gradualmente hasta detenerse.']
])

%rutinas nivel experto crossfit
respuestas_db(random_ec,[
	['Estirar los brazos y piernas por unos 5 minutos y trotar 1km para calentar. Para calentar tendras 10 minutos para realizar la mayor cantidad de rondas de 20 saltos dobles y 10 saltos al cajon en la 
    altura intermedia. Luego 5 rondas de 1 arranque completo, 1 arranque con sentadilla y 1 arranque en posicion elevada. Para la rutina realiza un EMOM donde cada minuto debes completar 2 arranques con sentadilla
    con un peso de 145lb y 10 saltos al cajon en la altura mas alta. La duracion sera de 10 minutos para un total de 10 rondas, en caso de no terminar una antes que termine el tiempo, debes dejarla donde quedo
    e iniciar con los 2 arranques de una vez. Al finalizar estira completamente y recuerda hidratarte bien.'],
	['Estirar los brazos y piernas por unos 5 minutos y hacer saltos dobles por 10 minutos para calentar. Luego realiza sentadillas con la barra en la espalda, inciando con un peso del 70% e incrementando hasta llegar
    a tu RM (peso maximo). Despues realiza un tabata de 16 rondas con lagartijas, plancha y v-ups. Para la rutina, sera un AMRAP donde la ronda consiste en 15 sentadillas con la barra al frente con 155lb, 15 dips en los aros 
    y correr 200m. El tiempo para realizar la mayor cantidad de ronas posible serian 15 minutos. Al finalizar recuerda estirar bien e hidratarte bien.']
])

%rutinas nivel experto halterofilia
respuestas_db(random_eh,[
	['Para calentar inicia trotando unos 500m, luego con la barra, realiza 10 rondas de burgener para clean para ir calentando el movimiento. Luego realiza 10 rondas de 5 repeticiones de 
    levantamientos arriba de la cabeza, lentamente para ir calentando el movimiento. Por ultimo, realiza 40 repeticiones de levantamientos clean y jerk con un peso del 50% de tu RM en el movimiento de jerk.']
])

%reglas de 

respuesta(hernia,[
	['Evita usar mucho peso o moverte con mucha intensidad, corta las repeticiones y la intensidad a la mitad de lo
	que deberias hacerlo']
])


?-mrtrainer. % Ejecuta el programa
