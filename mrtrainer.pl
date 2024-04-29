
% Importacion de la libreria random

 :- use_module(library(random)).

% Regla principal que llama mensaje de bienvenida y procesa la
% conversaciï¿½n
mrtrainer:-
	inicio_conversacion.

% Regla que crea un loop para procesar la conversaciï¿½n hasta que el
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
oracion_input(Input):- oracion_input_aux(L), filtro_input(L,Input).

% Regla auxiliar a oracion_input que recibe oraciÃ³n filtrada y la convierte en una lista.
oracion_input_aux(P):- initread(L), palabras(P, L, []).

% Lee el primer y segundo caracter ingresado por el usuario, llama a leer el
% resto de la lista
initread([K1,K2|U]):- get_code(K1),get_code(K2),leer_resto(K2,U).

% ---------------Reglas que procesan la lectura de caracteres-------

% Regla que revisa cada elemento de una lista que corresponde a un
% carácter de la oración y retorna una lista que contiene cada palabra.
filtro_input([],[]).
filtro_input(['\n'|T], Respuesta):- !, filtro_input(T, Respuesta).
filtro_input([num(2), X|T], [Rm|Respuesta]):-
		name(X, CharList),
        check_chars(CharList),!,
        name(Rm, [50 | CharList]),
        filtro_input(T, Respuesta).
filtro_input([X|T], [X|Respuesta]):-
        filtro_input(T, Respuesta).

% Si se recibe oraciï¿½n, procesar palabras recursivamente
palabras([V|U]) --> palabras_aux(V),!, blanks, palabras(U).
% Si no hay nada, regresa vacï¿½o
palabras([]) --> [].

% Procesa palabra en ASCII
palabras_aux(U1) --> [K],{convert_char(K,K1)},!,alfanum(U2),{name(U1,[K1|U2])}.
palabras_aux(num(N)) --> [K],{digit(K)},!,digito(U),{name(N,[K|U])}.
palabras_aux(V) --> [K],{name(V,[K])}.

% Genera recursivamente caracteres alfanumï¿½ricos en base a los cï¿½digos
% recibidos
alfanum([K1|U]) --> [K],{alfanum_aux(K,K1)},!,alfanum(U).
alfanum([]) --> [].

% Regla que llama a digit para verificar el elemento y luego utilizando 
% convert_char realiza la suma necesaria para obtener el carÃ¡cter alfanumÃ©rico.
alfanum_aux(95,95) :- !.
alfanum_aux(K,K1):- convert_char(K,K1).
alfanum_aux(K,K):- digit(K).

% Si es un espacio o otra seï¿½al desconocida
blanks --> [K], {K=<32},!, blanks.
% De lo contrario, son vacï¿½os
blanks --> [].

% Regla que procesa 113 caracteres como mÃ¡ximo mientras estÃ©n dentro 
% del rango ASCII del abecedario.
check_chars([113, X|_]):- digit(X).

% Si el caracter es un nï¿½mero, se retorna
digits([K|U]) --> [K],{digit(K)},!,digits(U).
% Si es vacï¿½o se regresa lista vacï¿½a
digits([]) --> [].

% Verifica si es un nï¿½mero
digit(K):- K>47, K<58.

% Procesa letras en ASCII, mayï¿½sculas y minï¿½sculas
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

% Respuesta si usuario dijo algï¿½n tipo de saludo
gen_respuesta(Input, Respuesta):-
	saludando(Input), !,
	respuestas_db(saludo, Res),
	selec_rand(Res, Respuesta).

% Respuesta si usuario dijo algï¿½n tipo de gracias
gen_respuesta(Input, Respuesta):-
	agradecimiento(Input), !,
	respuestas_db(gracias, Res),
	selec_rand(Res, Respuesta).

% Respuesta si usuario dijo algï¿½n tipo de despedida
gen_respuesta(Input, Respuesta):-
	despedida(Input), !,
	respuestas_db(bye, Res),
	selec_rand(Res, Respuesta).

gen_respuesta(Input, Respuesta):-
	oracion(Tree1, Input, _Rest), !,
	mapping(inicio_ases,Tree1, Tree2),
	question(Tree2, Rep,[]),
	append(Rep, ['?'], Respuesta).

% Regla que regresa a partir de una lista el valor del ï¿½ndice
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
% gramática libre de contexto ------------------------
%oracion(s(X,Y, is, Z)) --> belonging_phrase(X), abstract_noun(Y),
                            % [is],  special_noun(Z).

oracion(s(X, Y, Z))--> subject_phrase(X),verb(Y),noun(Z).

%oracion(s(X, Y, Z)) --> subject_phrase(X), verb(Y), object_phrase(Z).

%oracion(s(X, Y, Z)) --> question(X), object_pronoun(Y), noun(Z).

belonging_phrase(belong(tuyo)) --> [tuyo].
belonging_phrase(belong(mi)) --> [mi].

abstract_noun(abs_noun(name)) --> [name].

special_noun(sp_noun(justin)) --> [justin].
special_noun(sp_noun(frank)) --> [frank].

subject_phrase(sp(X)) --> proname_suj(X).
subject_phrase(sp(X)) --> noun_phrase(X).

object_phrase(op(X,Y)) --> noun_phrase(X), adverb(Y).
object_phrase(op(X, Y)) --> object_pronoun(X), adverb(Y).

noun_phrase(np(X, Y)) --> determiner(X), noun(Y).
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

object_pronoun(opn(you))--> [you].
object_pronoun(opn(tuyo))--> [tuyo].
object_pronoun(opn(me))--> [me].
object_pronoun(opn(us))--> [us].
object_pronoun(opn(them))--> [them].
object_pronoun(opn(him))--> [him].
object_pronoun(opn(her))--> [her].
object_pronoun(opn(it))--> [it].

determiner(dtmnr([])) --> [].
determiner(dtmnr([a])) --> [a].
determiner(dtmnr([the])) --> [the].
determiner(dtmnr([mi])) --> [mi].
determiner(dtmnr([some])) --> [some].
determiner(dtmnr([all])) --> [all].
determiner(dtmnr([that])) --> [that].

noun(noun(unosotros)) --> [unosotros].
noun(noun(cs_course)) --> [cs_course].
noun(noun(robotics_course)) --> [robotics_course].
noun(noun(robotics_course)) --> [computing_course].
noun(noun(robotics_course)) --> [sd_course].
noun(noun(name)) --> [name].
noun(noun(ejercicio)) --> [ejercicio].


adverb(ad([very, much])) --> [very, much].
adverb(ad([how])) --> [how].
adverb(ad([])) --> [].

verb(vb(gusta)) --> [gusta].
verb(vb(amo)) --> [amo].
verb(vb(es)) --> [es].
verb(vb(tengo))-->[tengo].

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
		s(sp(spn(X)),vb(Y1), noun(Z)),
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
busq_clave([], _, []).
busq_clave([H|T1], L2, [H|T3]):-
        member(H, L2), !,
        busq_clave(T1, L2, T3).
busq_clave([_|T1], L2, L3):-
        busq_clave(T1, L2, L3).

% ----------- Reglas para manejar condiciones de mensaje ---------

% Regla que verifica si se mandï¿½ mensaje de despedida en la oraciï¿½n
despedida(Input):-
	despedida_db(D),
	busq_clave(Input, D, A),
	A \== [].

% Regla que verifica si se mandï¿½ mensaje de gracias en la oraciï¿½n
agradecimiento(Input):-
	gracias_db(D),
	busq_clave(Input, D, A),
	A \== [].

% Regla que verifica si hay signo de pregunta en la oraciï¿½n
cuestionamiento(Input):-
	member('?', Input).

% Regla que verifica si se mandï¿½ mensaje de saludo en la oraciï¿½n
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