
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
	imprimir_nombre(tu), %impresion de palabra "cliente en pantalla"
	oracion_input(Input),
	gen_respuesta(Input,Respuesta),
	imprimir_nombre(yo),
	write_list(Respuesta),
	despedida(Input), !, halt.

mr_trainer('MrTrainer').
cliente('Cliente').

% Regla que imprime mensaje de lista en terminal
write_list([]):- nl.
write_list([H|T]):- write(H), write(' '), write_list(T).

% Regla que llama al name del usuario chat para simular interfaz de
% chat
imprimir_nombre(yo):- %impresion de "mr trainer en pantalla"
        mr_trainer(X), write(X), write(': '), flush_output.

% Regla que llama al name del usuario que utiliza el programa para
% simular interfaz de chat
imprimir_nombre(tu):-
        cliente(X), write(X), write(': '), flush_output.

% Regla que procesa la entrada del usuario.
oracion_input(Input):- oracion_input_aux(L), filtro_input(L, Input).

% Regla auxiliar a oracion_input que recibe oración filtrada y la convierte en una lista.
oracion_input_aux(P):- initread(L), palabras(P, L, []).

% Lee el primer y segundo caracter ingresado por el usuario, llama a leer el
% resto de la lista
initread([K1,K2|U]):- get_code(K1),get_code(K2),leer_resto(K2,U).

% ---------------Reglas que procesan la lectura de caracteres-------

% Regla que revisa cada elemento de una lista que corresponde a un
% car�cter de la oraci�n y retorna una lista que contiene cada palabra.
filtro_input([],[]).
filtro_input(['\n'|T], Respuesta):- !, filtro_input(T, Respuesta).
filtro_input([num(2), X|T], [Rm|Respuesta]):-
		name(X, CharList),
        check_chars(CharList),!,
        name(Rm, [50 | CharList]),
        filtro_input(T, Respuesta).
filtro_input([X|T], [X|Respuesta]):-
        filtro_input(T, Respuesta).

% Si se recibe oraci�n, procesar palabras recursivamente
palabras([V|U]) --> palabras_aux(V),!, vacio, palabras(U).
% Si no hay nada, regresa vac�o
palabras([]) --> [].

% Procesa palabra en ASCII
palabras_aux(U1) --> [K],{convert_char(K,K1)},!,alfanum(U2),{name(U1,[K1|U2])}.
palabras_aux(num(N)) --> [K],{digito_aux(K)},!,digito(U),{name(N,[K|U])}.
palabras_aux(V) --> [K],{name(V,[K])}.

% Genera recursivamente caracteres alfanum�ricos en base a los c�digos
% recibidos
alfanum([K1|U]) --> [K],{alfanum_aux(K,K1)},!,alfanum(U).
alfanum([]) --> [].

% Regla que llama a digito_aux para verificar el elemento y luego utilizando 
% convert_char realiza la suma necesaria para obtener el carácter alfanumérico.
alfanum_aux(95,95) :- !.
alfanum_aux(K,K1):- convert_char(K,K1).
alfanum_aux(K,K):- digito_aux(K).

% Si es un espacio o otra se�al desconocida
vacio --> [K], {K=<32},!, vacio.
% De lo contrario, son vac�os
vacio --> [].

% Regla que procesa 113 caracteres como máximo mientras estén dentro 
% del rango ASCII del abecedario.
check_chars([113, X|_]):- digito_aux(X).

% Si el caracter es un n�mero, se retorna
digito([K|U]) --> [K],{digito_aux(K)},!,digito(U).
% Si es vac�o se regresa lista vac�a
digito([]) --> [].

% Verifica si es un n�mero
digito_aux(K):- K>47, K<58.

% Procesa letras en ASCII, may�sculas y min�sculas
convert_char(K,K1):- K>64,K<91,!,K1 is K+32.
convert_char(K,K):- K>96,K<123.

% Llama recursivamente la palabra para leer sus letras
leer_resto(63,[]):- !.
leer_resto(33,[]):- !.
leer_resto(10,[]):- !.
leer_resto(K,[K1|U]):- K=<32,!,get_code(K1),leer_resto(K1,U).
leer_resto(_K1,[K2|U]):- get_code(K2),leer_resto(K2,U).

% --Reglas que procesan entrada del usuario y Respuesta de mrtrainer

% Respuesta si mrtrainer no entiende entrada
gen_respuesta(Input, Respuesta):-
\+ despedida(Input), \+ agradecimiento(Input), \+ saludando(Input), \+ oracion(Tree1, Input, _Rest),!,
respuestas_db(conector, Res), selec_rand(Res, Respuesta).

% Respuesta si usuario dijo alg�n tipo de saludo
gen_respuesta(Input, Respuesta):-
	saludando(Input), !,
	respuestas_db(saludo, Res),
	selec_rand(Res, Respuesta).

% Respuesta si usuario dijo alg�n tipo de gracias
gen_respuesta(Input, Respuesta):-
	agradecimiento(Input), !,
	respuestas_db(gracias, Res),
	selec_rand(Res, Respuesta).

% Respuesta si usuario dijo alg�n tipo de despedida
gen_respuesta(Input, Respuesta):-
	despedida(Input), !,
	respuestas_db(bye, Res),
	selec_rand(Res, Respuesta).

gen_respuesta(Input, Respuesta):-
	oracion(Tree1, Input, _Rest), !,
	mapping(inicio_ases,Tree1, Tree2),
	question(Tree2, Rep,[]),
	append(Rep, ['?'], Respuesta).



% Regla que regresa a partir de una lista el valor del �ndice
% especificado
nth_item([H|_], 1, H).
nth_item([_|T], N, X):-
	nth_item(T, N1, X),
	N is N1 + 1.

% Regla que escoge un valor aleatorio de una lista y lo retorna
selec_rand(Res, Respuesta):-
	length(Res, Length),
	Upper is Length + 1,
	random(1, Upper, Rand),
	nth_item(Res, Rand, Respuesta).

% ------------ Definiciones de Definite Clause Grammar para
% gram�tica libre de contexto ------------------------
%oracion(s(X,Y, is, Z)) --> belonging_phrase(X), abstract_noun(Y),
                            % [is],  special_noun(Z).

oracion(s(X, Y,Z,F,H))--> subject_phrase(X),verb(Y),complemento_d(Z),articulo(F),noun(H).
oracion(s(X, Y,Z,F,G,H))--> subject_phrase(X),verb(Y),complemento_d(Z),articulo(F),complemento_d(G),noun(H).
oracion(s(X, Y,Z,F,G,H))--> subject_phrase(X),verb(Y),complemento_d(Z),articulo(F),noun(H).
oracion(s(X, Y,Z,F,H))--> subject_phrase(X),verb(Y),complemento_d(Z),articulo(F),noun_inf(H).
oracion(s(X, Y,Z))--> subject_phrase(X),verb(Y),complemento_d(Z).
oracion(s(X, Y,F,Z))--> subject_phrase(X),verb(Y),articulo(F),noun(Z).
oracion(s(X, Y,F,Z))--> verb(X),complemento_d(Y),articulo(F),noun(Z).
oracion(s(X, Y,a,Z))--> verb(X),complemento_d(Y),[a],noun_inf(Z).
oracion(s(X, Y,Z))--> subject_phrase(X),verb(Y),noun_inf(Z).
oracion(s(si, Y,Z,G,H))--> [si],verb(Y),verb(Z),articulo(G),noun(H).

%oracion(s(X, Y, Z)) --> subject_phrase(X), verb(Y), object_phrase(Z).

%oracion(s(X, Y, Z)) --> question(X), object_pronoun(Y), noun(Z).


pertenencia(perte(tuyo)) --> [tuyo].
pertenencia(perte(mi)) --> [mi].


subject_phrase(sp(X)) --> proname_suj(X).
subject_phrase(sp(X)) --> noun_phrase(X).

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
noun(noun(peso)) --> [peso].

noun(noun(natacion)) --> [natacion].
noun(noun(ciclismo)) --> [ciclismo].
noun(noun(fondo)) --> [fondo].



noun(noun(rodilla)) --> [rodilla].
noun(noun(rodillas)) --> [rodillas].
noun(noun(articulaciones)) --> [articulaciones].
noun(noun(articulacion)) --> [articulacion].
noun(noun(tobillos)) --> [tobillos].
noun(noun(tobillo)) --> [tobillo].
noun(noun(munecas)) --> [munecas].

noun_inf(noun_inf(correr)) --> [correr].
noun_inf(noun_inf(nadar)) --> [nadar].
noun_inf(noun_inf(trotar)) --> [trotar].
noun_inf(noun_inf(saltar)) --> [saltar].


%Opciones de entrada de complemento directo
complemento_d(c_d(ganas)) --> [ganas].
complemento_d(c_d(empezar)) --> [empezar].
complemento_d(c_d(comenzar)) --> [comenzar].
complemento_d(c_d(aprender)) --> [aprender].
complemento_d(c_d(motivado)) --> [motivado].
complemento_d(c_d(arrancar)) --> [arrancar].
complemento_d(c_d(hacer)) --> [hacer].
complemento_d(c_d(correr)) --> [correr].


adverbio(ad([no])) --> [no].
adverbio(ad([si])) --> [si].


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
verb(vb(gustaria))-->[gustaria].
verb(vb(quisiera))-->[quisiera].







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
			s(sp(spn(X)),vb(Y1),c_d(Z),art(F),noun(H)),	
			q(excelente,iniciativa,caminar)
			).

mapping(inicio_ases,
			s(vb(X),c_d(Y1),art(F),noun(H)),	
			q(excelente,iniciativa,iniciemos)
			).
	
mapping(inicio_ases,
			s(vb(X),c_d(Y1),a,noun_inf(H)),	
			q(excelente,iniciativa,iniciemos)
			).


mapping(inicio_ases,
			s(si,vb(X),vb(Y),art(Z),noun(H)),	
			q(excelente,iniciativa,iniciemos)
			).





mapping(inicio_ases,
			s(sp(spn(X)),vb(Y1),c_d(Z),art(F),noun_inf(H)),	
			q(excelente,iniciativa,caminar)
			).

mapping(inicio_ases,
			s(sp(spn(X)),vb(Y1),noun_inf(H)),	
			q(excelente,iniciativa,caminar)
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
busq_clave([], _, []).
busq_clave([H|T1], L2, [H|T3]):-
        member(H, L2), !,
        busq_clave(T1, L2, T3).
busq_clave([_|T1], L2, L3):-
        busq_clave(T1, L2, L3).

% ----------- Reglas para manejar condiciones de mensaje ---------

% Regla que verifica si se mand� mensaje de despedida en la oraci�n
despedida(Input):-
	despedida_db(D),
	busq_clave(Input, D, A),
	A \== [].

% Regla que verifica si se mand� mensaje de gracias en la oraci�n
agradecimiento(Input):-
	gracias_db(D),
	busq_clave(Input, D, A),
	A \== [].

% Regla que verifica si hay signo de pregunta en la oraci�n
cuestionamiento(Input):-
	member('?', Input).

% Regla que verifica si se mand� mensaje de saludo en la oraci�n
saludando(Input):-
	saludo_db(D),
	busq_clave(Input, D, A),
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
respuestas_db(conector, [
	['Disculpa, no entendi lo que dices'],
	['Podrias repetir eso?'],
	['No estoy seguro de entender eso']
	]).

?-mrtrainer. % Ejecuta el programa