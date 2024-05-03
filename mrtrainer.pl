
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
	oracion(Tree1, Input, _Rest),	% Ojo que no tiene '!'
	mapping(inicio_ases, Tree1, Rep),
	append(Rep, [], Respuesta).

gen_respuesta(Input, Respuesta):-
	oracion(Tree1, Input, _Rest),
	mapping(buen_salud, Tree1, Respuesta),
	assertz(salud(bien)). % Hecho dinamico

gen_respuesta(Input, Respuesta):-
	oracion(Tree1, Input, _Rest),
	mapping(asmatico, Tree1, Respuesta),
	assertz(salud(asmatico)). % Hecho dinamico

gen_respuesta(Input, Respuesta):-
	oracion(Tree1, Input, _Rest),
	mapping(hipertension, Tree1, Respuesta),
	assertz(salud(hipertension)). % Hecho dinamico

gen_respuesta(Input, Respuesta):-
	oracion(Tree1, Input, _Rest),
	mapping(hernia, Tree1, Respuesta),
	assertz(salud(hernia)). % Hecho dinamico

gen_respuesta(Input, Respuesta):-
	oracion(Tree1, Input, _Rest),
	mapping(quebradurapierna, Tree1, Respuesta),
	assertz(salud(quebradurapierna)). % Hecho dinamico

gen_respuesta(Input, Respuesta):-
	oracion(Tree1, Input, _Rest),
	mapping(quebradurabrazo, Tree1, Respuesta),
	assertz(salud(quebradurabrazo)). % Hecho dinamico

gen_respuesta(Input, Respuesta):-
	oracion(Tree1, Input, _Rest),
	mapping(cero, Tree1, Respuesta),
	assertz(nivel(principiante)), % Hecho dinamico
	salud(Status),
	deporte(Sport),
	nivel(Estado),
	rutinas_db(Sport, Estado, Status, Rutina),
    write(Rutina),
	nl,
	retractall(salud(_)),
	retractall(deporte(_)),
	retractall(nivel(_)).

gen_respuesta(Input, Respuesta):-
	oracion(Tree1, Input, _Rest),
	mapping(uno, Tree1, Respuesta),
	assertz(nivel(principiante)), % Hecho dinamico
	salud(Status),
	deporte(Sport),
	nivel(Estado),
	rutinas_db(Sport, Estado, Status, Rutina),
    write(Rutina),
	nl,
	retractall(salud(_)),
	retractall(deporte(_)),
	retractall(nivel(_)).

gen_respuesta(Input, Respuesta):-
	oracion(Tree1, Input, _Rest),
	mapping(dos, Tree1, Respuesta),
	assertz(nivel(principiante)), % Hecho dinamico
	salud(Status),
	deporte(Sport),
	nivel(Estado),
	rutinas_db(Sport, Estado, Status, Rutina),
    write(Rutina),
	nl,
	retractall(salud(_)),
	retractall(deporte(_)),
	retractall(nivel(_)).

gen_respuesta(Input, Respuesta):-
	oracion(Tree1, Input, _Rest),
	mapping(tres, Tree1, Respuesta),
	assertz(nivel(intermedio)), % Hecho dinamico
	salud(Status),
	deporte(Sport),
	nivel(Estado),
	rutinas_db(Sport, Estado, Status, Rutina),
    write(Rutina),
	nl,
	retractall(salud(_)),
	retractall(deporte(_)),
	retractall(nivel(_)).

gen_respuesta(Input, Respuesta):-
	oracion(Tree1, Input, _Rest),
	mapping(cuatro, Tree1, Respuesta),
	assertz(nivel(intermedio)), % Hecho dinamico
	salud(Status),
	deporte(Sport),
	nivel(Estado),
	rutinas_db(Sport, Estado, Status, Rutina),
    write(Rutina),
	nl,
	retractall(salud(_)),
	retractall(deporte(_)),
	retractall(nivel(_)).

gen_respuesta(Input, Respuesta):-
	oracion(Tree1, Input, _Rest),
	mapping(cinco, Tree1, Respuesta),
	assertz(nivel(avanzado)), % Hecho dinamico
	salud(Status),
	deporte(Sport),
	nivel(Estado),
	rutinas_db(Sport, Estado, Status, Rutina),
    write(Rutina),
	nl,
	retractall(salud(_)),
	retractall(deporte(_)),
	retractall(nivel(_)).

gen_respuesta(Input, Respuesta):-
	oracion(Tree1, Input, _Rest),
	mapping(seis, Tree1, Respuesta),
	assertz(nivel(avanzado)), % Hecho dinamico
	salud(Status),
	deporte(Sport),
	nivel(Estado),
	rutinas_db(Sport, Estado, Status, Rutina),
    write(Rutina),
	nl,
	retractall(salud(_)),
	retractall(deporte(_)),
	retractall(nivel(_)).

gen_respuesta(Input, Respuesta):-
	oracion(Tree1, Input, _Rest),
	mapping(siete, Tree1, Respuesta),
	assertz(nivel(avanzado)), % Hecho dinamico
	salud(Status),
	deporte(Sport),
	nivel(Estado),
	rutinas_db(Sport, Estado, Status, Rutina),
    write(Rutina),
	nl,
	retractall(salud(_)),
	retractall(deporte(_)),
	retractall(nivel(_)).

gen_respuesta(Input, Respuesta):-
	oracion(Tree1, Input, _Rest),
	mapping(neumonia, Tree1, Respuesta),
	assertz(salud(neumonia)). % Hecho dinamico

gen_respuesta(Input, Respuesta):-
	oracion(Tree1, Input, _Rest),
	mapping(problemas_salud_crossfit, Tree1, Respuesta),
	assertz(deporte(crossfit)). % Hecho dinamico
	
gen_respuesta(Input, Respuesta):-
	oracion(Tree1, Input, _Rest),
	mapping(problemas_salud_halterofilia, Tree1, Respuesta),
	assertz(deporte(halterofilia)). % Hecho dinamico

gen_respuesta(Input, Respuesta):-
	oracion(Tree1, Input, _Rest),
	mapping(problemas_salud_atletismo, Tree1, Respuesta),!,
	assertz(deporte(atletismo)). % Hecho dinamico




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
oracion(s(X, Y,G,Z))--> subject_phrase(X),verb(Y),verb(G),noun(Z).
oracion(s(si, Y,Z,G,H))--> [si],verb(Y),verb(Z),articulo(G),noun(H).
oracion(s(Z,G))--> verb(Z),noun_inf(G).
oracion(s(no, X, nada))-->[no],verb(X),[nada].
oracion(s(no, X, ningun, padecimiento))-->[no],verb(X),[ningun],[padecimiento].
oracion(s(X, asma))-->verb(X), [asma].
oracion(s(X, asmatico))-->verb(X), [asmatico].
oracion(s(X, hipertension))-->verb(X), [hipertension].
oracion(s(X, Y, hernia))-->verb(X), articulo(Y), [hernia].
oracion(s(X, Y, pierna, B))-->verb(X), articulo(Y), [pierna], adjetivo(B).
oracion(s(X, Y, piernas, B))-->verb(X), articulo(Y), [piernas], adjetivo(B).
oracion(s(X, pierna, Z, B))-->articulo(X), [pierna], verb(Z), adjetivo(B).
oracion(s(X, piernas, Z, B))-->articulo(X), [piernas], verb(Z), adjetivo(B).
oracion(s(X, Y, brazo, B))-->verb(X), articulo(Y), [brazo], adjetivo(B).
oracion(s(X, Y, brazos, B))-->verb(X), articulo(Y), [brazos], adjetivo(B).
oracion(s(X, brazo, Z, B))-->articulo(X), [brazo], verb(Z), adjetivo(B).
oracion(s(X, brazos, Z, B))-->articulo(X), [brazos], verb(Z), adjetivo(B).
oracion(s(X, neumonia))-->verb(X), [neumonia].
oracion(s(cerol, X))-->[0], noun(X).
oracion(s(cero, X))-->[cero], noun(X).
oracion(s(1, X))-->[1], noun(X).
oracion(s(un, X))-->[un], noun(X).
oracion(s(una, X))-->[una], noun(X).
oracion(s(2, X))-->[2], noun(X).
oracion(s(dos, X))-->[dos], noun(X).
oracion(s(3, X))-->[3], noun(X).
oracion(s(tres, X))-->[tres], noun(X).
oracion(s(4, X))-->[4], noun(X).
oracion(s(cuatro, X))-->[cuatro], noun(X).
oracion(s(5, X))-->[5], noun(X).
oracion(s(cinco, X))-->[cinco], noun(X).
oracion(s(6, X))-->[6], noun(X).
oracion(s(seis, X))-->[seis], noun(X).
oracion(s(7, X))-->[7], noun(X).
oracion(s(siete, X))-->[siete], noun(X).


%Entradas para crossfit 
oracion(s(crossfit))--> [crossfit].
oracion(s(Z,G,crossfit))--> verb(Z),noun_inf(G),[crossfit].
oracion(s(Z,G,H,crossfit))--> verb(Z),noun_inf(G),articulo(H),[crossfit].
oracion(s(X, Y,G,crossfit))--> subject_phrase(X),verb(Y),verb(G),[crossfit].
oracion(s(X,Y,crossfit))--> verb(X),complemento_d(Y),[crossfit].

%Entradas para atletismo 
oracion(s(atletismo))--> [atletismo].
oracion(s(Z,G,F,H,atletismo))--> verb(Z),noun_inf(G),articulo(F),articulo(H),[atletismo].
oracion(s(Z,G,H,atletismo))--> verb(Z),noun_inf(G),articulo(H),[atletismo].
oracion(s(X, Y,G,atletismo))--> subject_phrase(X),verb(Y),verb(G),[atletismo].
oracion(s(X,Y,atletismo))--> verb(X),complemento_d(Y),[atletismo].
oracion(s(X,Y,atletismo))--> verb(X),noun_inf(Y),[atletismo].

%Entradas para halterofilia
oracion(s(halterofilia))--> [halterofilia].
oracion(s(Z,G,F,H,halterofilia))--> verb(Z),noun_inf(G),articulo(F),articulo(H),[halterofilia].
oracion(s(Z,G,H,halterofilia))--> verb(Z),noun_inf(G),articulo(H),[halterofilia].
oracion(s(X, Y,G,halterofilia))--> subject_phrase(X),verb(Y),verb(G),[halterofilia].
oracion(s(X,Y,halterofilia))--> verb(X),complemento_d(Y),[halterofilia].
oracion(s(X,Y,halterofilia))--> verb(X),noun_inf(Y),[halterofilia].

oracion(s(X,Y,Z,G))--> verb(X),articulo(Y),noun(Z),nivel(G).





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
articulo(art([los])) --> [los].
articulo(art([uno])) --> [uno].
articulo(art([unos])) --> [unos].

nivel(niv([basica])) --> [basica].
nivel(niv([sencilla])) --> [sencilla].
nivel(niv([facil])) --> [facil].


nivel(niv([intermedia])) --> [intermedia].
nivel(niv([avanzada])) --> [avanzada].



noun(noun(ejercicio)) --> [ejercicio].
noun(noun(padecimiento)) --> [padecimiento].
noun(noun(dolor)) --> [dolor].
noun(noun(molestia)) --> [molestia].
noun(noun(problema)) --> [problema].
noun(noun(peso)) --> [peso].
noun(noun(rutina)) --> [rutina].

noun(noun(crossfit)) --> [crossfit].
noun(noun(halterofilia)) --> [halterofilia].
noun(noun(atletismo)) --> [atletismo].


noun(noun(deporte)) --> [deporte].
noun(noun(salud)) --> [salud].
noun(noun(rutina)) --> [rutina].
noun(noun(rodilla)) --> [rodilla].
noun(noun(rodillas)) --> [rodillas].
noun(noun(articulaciones)) --> [articulaciones].
noun(noun(articulacion)) --> [articulacion].
noun(noun(tobillos)) --> [tobillos].
noun(noun(tobillo)) --> [tobillo].
noun(noun(munecas)) --> [munecas].

noun(noun(dia)) --> [dia].
noun(noun(dias)) --> [dias].
noun(noun(vez)) --> [vez].
noun(noun(veces)) --> [veces].


noun_inf(noun_inf(correr)) --> [correr].
noun_inf(noun_inf(caminar)) --> [caminar].
noun_inf(noun_inf(nadar)) --> [nadar].
noun_inf(noun_inf(trotar)) --> [trotar].
noun_inf(noun_inf(saltar)) --> [saltar].
noun_inf(noun_inf(ejercitarme)) --> [ejercitarme].
noun_inf(noun_inf(moverme)) --> [moverme].
noun_inf(noun_inf(practicar)) --> [practicar].


%Opciones de entrada de complemento directo
complemento_d(c_d(ganas)) --> [ganas].
complemento_d(c_d(empezar)) --> [empezar].
complemento_d(c_d(comenzar)) --> [comenzar].
complemento_d(c_d(aprender)) --> [aprender].
complemento_d(c_d(motivado)) --> [motivado].
complemento_d(c_d(arrancar)) --> [arrancar].
complemento_d(c_d(hacer)) --> [hacer].
complemento_d(c_d(correr)) --> [correr].

complemento_d(c_d(mejorar))-->[mejorar].
complemento_d(c_d(aumentar))-->[aumentar].
complemento_d(c_d(bajar))-->[bajar].
complemento_d(c_d(subir))-->[subir].


adverbio(ad([no])) --> [no].
adverbio(ad([si])) --> [si].


%Definicion de verbos  
verb(vb(gusta)) --> [gusta].
verb(vb(amo)) --> [amo].
verb(vb(es)) --> [es].
verb(vb(tengo))-->[tengo].
verb(vb(quiero))-->[quiero].
verb(vb(soy))-->[soy].
verb(vb(hacer))-->[hacer].


verb(vb(deseo))-->[deseo].
verb(vb(necesito))-->[necesito].
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

verb(vb(esta))-->[esta].
verb(vb(estan))-->[estan].

%Definicion de adjetivos
adjetivo(adj(roto))-->[roto].
adjetivo(adj(rotos))-->[rotos].
adjetivo(adj(rota))-->[rota].
adjetivo(adj(rotas))-->[rotas].
adjetivo(adj(quebrado))-->[quebrado].
adjetivo(adj(quebrados))-->[quebrados].

%mapping de crossfit
mapping(problemas_salud_crossfit,
			s(crossfit), Respuesta):-
			respuestas_db(enfermedad, Res), selec_rand(Res, Respuesta).

mapping(problemas_salud_crossfit,
			s(vb(X),noun_inf(Y),crossfit), Respuesta):-
			respuestas_db(enfermedad, Res), selec_rand(Res, Respuesta).

mapping(problemas_salud_crossfit,
			s(vb(X),noun_inf(Y), art([Z]), crossfit), Respuesta):-
			respuestas_db(enfermedad, Res), selec_rand(Res, Respuesta).

mapping(problemas_salud_crossfit,
			s(vb(X),c_d(Y), crossfit), Respuesta):-
			respuestas_db(enfermedad, Res), selec_rand(Res, Respuesta).

mapping(problemas_salud_crossfit,
			s(sp(X),vb(Y),vb(Z),crossfit), Respuesta):-
			respuestas_db(enfermedad, Res), selec_rand(Res, Respuesta).

%mapping de atletismo
mapping(problemas_salud_atletismo,
			s(atletismo), Respuesta):-
			respuestas_db(enfermedad, Res), selec_rand(Res, Respuesta).

mapping(problemas_salud_atletismo,
			s(vb(X),noun_inf(Y),art([Z]),art([W]),atletismo), Respuesta):-
			respuestas_db(enfermedad, Res), selec_rand(Res, Respuesta).

mapping(problemas_salud_atletismo,
			s(vb(X),noun_inf(Y),atletismo), Respuesta):-
			respuestas_db(enfermedad, Res), selec_rand(Res, Respuesta).

mapping(problemas_salud_atletismo,
			s(vb(X),noun_inf(Y),art([Z]),atletismo), Respuesta):-
			respuestas_db(enfermedad, Res), selec_rand(Res, Respuesta).

mapping(problemas_salud_atletismo,
			s(vb(X),c_d(Y), atletismo), Respuesta):-
			respuestas_db(enfermedad, Res), selec_rand(Res, Respuesta).

mapping(problemas_salud_atletismo,
			s(sp(X),vb(Y),vb(Z),atletismo), Respuesta):-
			respuestas_db(enfermedad, Res), selec_rand(Res, Respuesta).

%mapping de halterofilia
mapping(problemas_salud_halterofilia,
			s(halterofilia), Respuesta):-
			respuestas_db(enfermedad, Res), selec_rand(Res, Respuesta).

mapping(problemas_salud_halterofilia,
			s(vb(X),noun_inf(Y),art([Z]),art([W]),halterofilia), Respuesta):-
			respuestas_db(enfermedad, Res), selec_rand(Res, Respuesta).

mapping(problemas_salud_halterofilia,
			s(vb(X),c_d(Y), halterofilia), Respuesta):-
			respuestas_db(enfermedad, Res), selec_rand(Res, Respuesta).

mapping(problemas_salud_halterofilia,
			s(vb(X),noun_inf(Y),art([Z]),halterofilia), Respuesta):-
			respuestas_db(enfermedad, Res), selec_rand(Res, Respuesta).

mapping(problemas_salud_halterofilia,
			s(vb(X),noun_inf(Y),halterofilia), Respuesta):-
			respuestas_db(enfermedad, Res), selec_rand(Res, Respuesta).

mapping(problemas_salud_halterofilia,
			s(sp(X),vb(Y),vb(Z),halterofilia), Respuesta):-
			respuestas_db(enfermedad, Res), selec_rand(Res, Respuesta).

%mapping condiciones de salud
%mapping de bien de salud
mapping(buen_salud,
			s(no, vb(X), nada), Respuesta):-
			respuestas_db(salud, Res), selec_rand(Res, Respuesta).

mapping(buen_salud,
			s(no, vb(X), ningun, padecimiento), Respuesta):-
			respuestas_db(salud, Res), selec_rand(Res, Respuesta).

%mapping de salud asmatico
mapping(asmatico,
			s(vb(X), asma), Respuesta):-
			respuestas_db(salud, Res), selec_rand(Res, Respuesta).

mapping(asmatico,
			s(vb(X), asmatico), Respuesta):-
			respuestas_db(salud, Res), selec_rand(Res, Respuesta).

%mapping de salud hipertension
mapping(hipertension,
			s(vb(X), hipertension), Respuesta):-
			respuestas_db(salud, Res), selec_rand(Res, Respuesta).

%mapping de salud hernia
mapping(hernia,
			s(vb(X), art(Y), hernia), Respuesta):-
			respuestas_db(salud, Res), selec_rand(Res, Respuesta).

%mapping de salud quebradura de pierna
mapping(quebradurapierna,
			s(vb(X), art([Y]), pierna, adj(B)), Respuesta):-
			respuestas_db(salud, Res), selec_rand(Res, Respuesta).

mapping(quebradurapierna,
			s(vb(X), art([Y]), piernas, adj(B)), Respuesta):-
			respuestas_db(salud, Res), selec_rand(Res, Respuesta).

mapping(quebradurapierna,
			s(art([X]), pierna, vb(Y), adj(B)), Respuesta):-
			respuestas_db(salud, Res), selec_rand(Res, Respuesta).

mapping(quebradurapierna,
			s(art([X]), piernas, vb(Y), adj(B)), Respuesta):-
			respuestas_db(salud, Res), selec_rand(Res, Respuesta).

%mapping de salud quebradura de brazo
mapping(quebradurabrazo,
			s(vb(X), art([Y]), brazo, adj(B)), Respuesta):-
			respuestas_db(salud, Res), selec_rand(Res, Respuesta).

mapping(quebradurabrazo,
			s(vb(X), art([Y]), brazos, adj(B)), Respuesta):-
			respuestas_db(salud, Res), selec_rand(Res, Respuesta).

mapping(quebradurabrazo,
			s(art([X]), brazo, vb(Y), adj(B)), Respuesta):-
			respuestas_db(salud, Res), selec_rand(Res, Respuesta).

mapping(quebradurabrazo,
			s(art([X]), brazos, vb(Y), adj(B)), Respuesta):-
			respuestas_db(salud, Res), selec_rand(Res, Respuesta).

%mapping de salud neumonia
mapping(neumonia,
			s(vb(X), neumonia), Respuesta):-
			respuestas_db(salud, Res), selec_rand(Res, Respuesta).

%mapping de numeros
mapping(cero,
			s(cerol, noun(X)), Respuesta):-
			respuestas_db(rutina, Res), selec_rand(Res, Respuesta).

mapping(cero,
			s(cero, noun(X)), Respuesta):-
			respuestas_db(rutina, Res), selec_rand(Res, Respuesta).
	
mapping(uno,
			s(1, noun(X)), Respuesta):-
			respuestas_db(rutina, Res), selec_rand(Res, Respuesta).

mapping(uno,
			s(un, noun(X)), Respuesta):-
			respuestas_db(rutina, Res), selec_rand(Res, Respuesta).

mapping(uno,
			s(una, noun(X)), Respuesta):-
			respuestas_db(rutina, Res), selec_rand(Res, Respuesta).

mapping(dos,
			s(2, noun(X)), Respuesta):-
			respuestas_db(rutina, Res), selec_rand(Res, Respuesta).

mapping(dos,
			s(dos, noun(X)), Respuesta):-
			respuestas_db(rutina, Res), selec_rand(Res, Respuesta).

mapping(tres,
			s(3, noun(X)), Respuesta):-
			respuestas_db(rutina, Res), selec_rand(Res, Respuesta).

mapping(tres,
			s(tres, noun(X)), Respuesta):-
			respuestas_db(rutina, Res), selec_rand(Res, Respuesta).

mapping(cuatro,
			s(4, noun(X)), Respuesta):-
			respuestas_db(rutina, Res), selec_rand(Res, Respuesta).

mapping(cuatro,
			s(cuatro, noun(X)), Respuesta):-
			respuestas_db(rutina, Res), selec_rand(Res, Respuesta).

mapping(cinco,
			s(5, noun(X)), Respuesta):-
			respuestas_db(rutina, Res), selec_rand(Res, Respuesta).

mapping(cinco,
			s(cinco, noun(X)), Respuesta):-
			respuestas_db(rutina, Res), selec_rand(Res, Respuesta).

mapping(seis,
			s(6, noun(X)), Respuesta):-
			respuestas_db(rutina, Res), selec_rand(Res, Respuesta).

mapping(seis,
			s(seis, noun(X)), Respuesta):-
			respuestas_db(rutina, Res), selec_rand(Res, Respuesta).

mapping(siete,
			s(7, noun(X)), Respuesta):-
			respuestas_db(rutina, Res), selec_rand(Res, Respuesta).

mapping(siete,
			s(siete, noun(X)), Respuesta):-
			respuestas_db(rutina, Res), selec_rand(Res, Respuesta).

%mapping(inicio_ases,
%			s(si,vb(X),vb(Y),art(Z),noun(H)),	
%			[excelente, iniciativa, iniciemos]
%			).


%mapping(inicio_ases,
%			s(sp(spn(X)),vb(Y1),c_d(Z),art(F),noun_inf(H)),	
%			[excelente, iniciativa, iniciemos]
%			).

%mapping(inicio_ases,
%			s(sp(spn(X)),vb(Y1),noun_inf(H)),	
%			[excelente, iniciativa, iniciemos]
%			).


%mapping(inicio_ases,
%			s(sp(spn(X)),vb(Y1),vb(Z),noun_inf(H)),	
%			[excelente, iniciativa, iniciemos]
%			).

%mapping(inicio_ases,
%			s(vb(Y1),noun_inf(H)),	
%			[excelente, iniciativa, iniciemos]
%			).

%mapping(inicio_ases,
%			s(vb(X),art(Y),noun(Z),niv(G)),	
%			[excelente, iniciativa, iniciemos]
%			).
	


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
	['Hola, quieres practicar crossfit, atletismo o halterofilia?'],
	['Hola, listo para ayudar'],
	['Buenas, que deporte practicas?'],
	['Hola soy Mr. Trainer, vamos a entrenar :D']
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


% Posibles mensajes para preguntar padecimientos
respuestas_db(enfermedad, [
	['Tiene algun padecimiento?'],
	['Tiene alguna enfermedad que afecte en su salud fisica?'],
	['Tiene alguna patologia que le impida ejercitarse con normalidad?']
	]).

% Posibles mensajes para preguntar dias de ejercicio por semana
respuestas_db(salud, [
	['En una semana cuanto te ejercitas?'],
	['Cuantas veces te ejercitas por semana?'],
	['Semanalmente cuanta actividad fisica haces?'],
	['Te ejercitas muchos dias en una semana?']
	]).

% Posibles mensajes para presentar rutina
respuestas_db(rutina, [
	['Esta podria ser tu rutina'],
	['Con base en los datos que me diste te puedo sugerir esta rutina'],
	['Me parece que esta rutina se adapta a tus necesidades']
	]).
prueba_db(test, [['Este es un string predefinido, puede ser random']]).

prueba2_db(test, [['¿Tienes algún problema de salud?']]).

prueba3_db(test, hola).



rutinas_db(atletismo, principiante, X).
rutinas_db(atletismo, intermedio, X).
rutinas_db(atletismo, avanzado, X).


rutinas_db(crossfit, principiante, X).
rutinas_db(crossfit, intermedio, X).
rutinas_db(crossfit, avanzado, X).

rutinas_db(halterofilia, principiante, X).
rutinas_db(halterofilia, intermedio, X).
rutinas_db(halterofilia, avanzado, X).

%rutinas principiante atletismo
rutinas_db(atletismo,principiante,bien,
	'Estirar por unos 5 minutos las piernas antes de iniciar. Luego dar 2 vueltas a la pista caminando a alta velocidad
    para calentar y dar zancadas por unos 100m. Para la rutina, corre por una distancia de 400 metros, empezando a un 25% de su capacidad para correr en los primeros 100
    metros y aumentando en 25% cada 100 metros, luego descansar por unos 30 segundos y repetir esta rutina unas 10 veces. Al terminar cada vuelta, no frene de golpe, mantengase
    caminando lentamente para evitar la acumulacion de acido lactico en las piernas.').

rutinas_db(atletismo,principiante,hipertension,
	'Estirar por unos 5 minutos las piernas antes de iniciar. Luego dar 2 vueltas a la pista caminando a alta velocidad
    para calentar y dar zancadas por unos 100m. Para la rutina, corre por una distancia de 400 metros, empezando a un 25% de su capacidad para correr en los primeros 100
    metros y aumentando en 25% cada 100 metros, luego descansar por unos 30 segundos y repetir esta rutina unas 10 veces. Al terminar cada vuelta, no frene de golpe, mantengase
    caminando lentamente para evitar la acumulacion de acido lactico en las piernas.').

rutinas_db(atletismo,principiante,hernia,
	'Estirar por unos 5 minutos las piernas antes de iniciar. Luego dar 2 vueltas a la pista caminando a alta velocidad
    para calentar y dar zancadas por unos 100m. Para la rutina, corre por una distancia de 400 metros, empezando a un 25% de su capacidad para correr en los primeros 100
    metros y aumentando en 25% cada 100 metros, luego descansar por unos 30 segundos y repetir esta rutina unas 10 veces. Al terminar cada vuelta, no frene de golpe, mantengase
    caminando lentamente para evitar la acumulacion de acido lactico en las piernas.').

rutinas_db(atletismo,principiante,quebradurabrazo,
	'Estirar por unos 5 minutos las piernas antes de iniciar. Luego dar 2 vueltas a la pista caminando a alta velocidad
    para calentar y dar zancadas por unos 100m. Para la rutina, corre por una distancia de 400 metros, empezando a un 25% de su capacidad para correr en los primeros 100
    metros y aumentando en 25% cada 100 metros, luego descansar por unos 30 segundos y repetir esta rutina unas 10 veces. Al terminar cada vuelta, no frene de golpe, mantengase
    caminando lentamente para evitar la acumulacion de acido lactico en las piernas.').

rutinas_db(atletismo,principiante,neumonia,
	'Estirar por unos 5 minutos las piernas antes de iniciar. Luego dar 1 vuelta a la pista caminando a una velocidad media
    para calentar y dar zancadas por unos 100m manteniendo un ritmo moderado. Para la rutina, correr por una distancia de 400 
    metros, empezando a un 25% de su capacidad para correr en los primeros 100 metros y aumentando al 60% de su capacidad en 
    los ultimos 200m, manteniendo un ritmo moderado y ligero, luego descansar por unos 30 segundos y repetir esta rutina unas 
    10 veces. Al terminar cada vuelta, no frene de golpe, mantengase caminando lentamente para evitar la acumulacion de acido lactico en las piernas.').

rutinas_db(atletismo,principiante,asmatico,
	'Estirar por unos 5 minutos las piernas antes de iniciar. Luego dar 1 vuelta a la pista caminando a una velocidad media
    para calentar y dar zancadas por unos 100m manteniendo un ritmo moderado. Para la rutina, correr por una distancia de 400 
    metros, empezando a un 25% de su capacidad para correr en los primeros 100 metros y aumentando al 60% de su capacidad en 
    los ultimos 200m, manteniendo un ritmo moderado y ligero, luego descansar por unos 30 segundos y repetir esta rutina unas 
    10 veces. Al terminar cada vuelta, no frene de golpe, mantengase caminando lentamente para evitar la acumulacion de acido lactico en las piernas.').

rutinas_db(atletismo,principiante,quebradurapierna,
	'No te recomiendo atletismo con una pierna rota, pero podrias hacer esta rutina: 
    Estirar por unos 5 minutos las piernas antes de iniciar. Luego camina dandole 1 vuelta a la pista para calentar. Luego has una pausa de unos
    segundos y repite esto unas 10 veces tratando en lo posible de ir con un paso un poco mas acelerado pero sin correr ni trotar para que almenos
    mantengas la fuerza en la pierna sana.').

%rutinas intermedio atletismo
rutinas_db(atletismo,intermedio, bien,
	'Estirar por unos 5 minutos las piernas antes de iniciar. Luegp para calentar, trotar por 400m incrementando la velocidad un 25% cada 100m 
    Al llegar a los 400m, mantener el ritmo por 800m y luego por 100m disminuir la velocidad hasta detenerse. Para la rutina hacer series de 500 metros
    almenos unas 10 series. Al terminar, recuerda estirar y bajar la velocidad gradualmente para evitar lesiones.').

rutinas_db(atletismo,intermedio,hipertension,
	'Estirar por unos 5 minutos las piernas antes de iniciar. Luegp para calentar, trotar por 400m incrementando la velocidad un 25% cada 100m 
    Al llegar a los 400m, mantener el ritmo por 800m y luego por 100m disminuir la velocidad hasta detenerse. Para la rutina hacer series de 500 metros
    almenos unas 10 series. Al terminar, recuerda estirar y bajar la velocidad gradualmente para evitar lesiones.').

rutinas_db(atletismo,intermedio,hernia,
	'Estirar por unos 5 minutos las piernas antes de iniciar. Luegp para calentar, trotar por 400m incrementando la velocidad un 25% cada 100m 
    Al llegar a los 400m, mantener el ritmo por 800m y luego por 100m disminuir la velocidad hasta detenerse. Para la rutina hacer series de 500 metros
    almenos unas 10 series. Al terminar, recuerda estirar y bajar la velocidad gradualmente para evitar lesiones.').

rutinas_db(atletismo,intermedio,quebradurabrazo,
	'Estirar por unos 5 minutos las piernas antes de iniciar. Luegp para calentar, trotar por 400m incrementando la velocidad un 25% cada 100m 
    Al llegar a los 400m, mantener el ritmo por 800m y luego por 100m disminuir la velocidad hasta detenerse. Para la rutina hacer series de 500 metros
    almenos unas 10 series. Al terminar, recuerda estirar y bajar la velocidad gradualmente para evitar lesiones.').

rutinas_db(atletismo,intermedio, neumonia,
	'Estirar por unos 5 minutos las piernas antes de iniciar. Luegp para calentar, trotar por 400m incrementando la velocidad un 25% cada 100m 
    Sin llegar a mas del 60% y sin hiperventilarte. Al llegar a los 400m, mantener el ritmo por 800m y luego por 
    100m disminuir la velocidad hasta detenerse. Descansa unos minutos para que recuperes el aire. Para la rutina 
    hacer series de 500 metros a un paso moderado, almenos unas 10 series. Al terminar, recuerda estirar y bajar 
    la velocidad gradualmente para evitar lesiones. Recuerda no acelerarte mucho para evitar dificultades al respirar
    por tu condicion y tomar pausas caminando de ser necesario').

rutinas_db(atletismo,intermedio, asmatico,
	'Estirar por unos 5 minutos las piernas antes de iniciar. Luegp para calentar, trotar por 400m incrementando la velocidad un 25% cada 100m 
    Sin llegar a mas del 60% y sin hiperventilarte. Al llegar a los 400m, mantener el ritmo por 800m y luego por 
    100m disminuir la velocidad hasta detenerse. Descansa unos minutos para que recuperes el aire. Para la rutina 
    hacer series de 500 metros a un paso moderado, almenos unas 10 series. Al terminar, recuerda estirar y bajar 
    la velocidad gradualmente para evitar lesiones. Recuerda no acelerarte mucho para evitar dificultades al respirar
    por tu condicion y tomar pausas caminando de ser necesario').

rutinas_db(atletismo,intermedio,quebradurapierna,
	'No te recomiendo atletismo con una pierna rota, pero podrias hacer esta rutina: 
    Estirar por unos 5 minutos las piernas antes de iniciar. Luego camina dandole 1 vuelta a la pista para calentar. Luego has una pausa de unos
    segundos y repite esto unas 10 veces tratando en lo posible de ir con un paso un poco mas acelerado pero sin correr ni trotar para que almenos
    mantengas la fuerza en la pierna sana.').

%rutinas avanzado atletismo
rutinas_db(atletismo,avanzado,bien,
	'Estirar por unos 5 minutos las piernas antes de iniciar. Luego para calentar da 2 vueltas trotando a una pista o por 1600m para calentar.
    Para la rutina realizar sprints de 100m con un descanso activo de 200m hasta completar 9km (un total de 30 veces). En la ultima ronda realizala
    al 100% de la velocidad posible y en los ultimo 200m disminuir la velocidad gradualmente hasta detenerse.').

rutinas_db(atletismo,avanzado,hipertension,
	'Estirar por unos 5 minutos las piernas antes de iniciar. Luego para calentar da 2 vueltas trotando a una pista o por 1600m para calentar.
    Para la rutina realizar sprints de 100m con un descanso activo de 200m hasta completar 9km (un total de 30 veces). En la ultima ronda realizala
    al 100% de la velocidad posible y en los ultimo 200m disminuir la velocidad gradualmente hasta detenerse.').

rutinas_db(atletismo,avanzado,hernia,
	'Estirar por unos 5 minutos las piernas antes de iniciar. Luego para calentar da 2 vueltas trotando a una pista o por 1600m para calentar.
    Para la rutina realizar sprints de 100m con un descanso activo de 200m hasta completar 9km (un total de 30 veces). En la ultima ronda realizala
    al 100% de la velocidad posible y en los ultimo 200m disminuir la velocidad gradualmente hasta detenerse.').

rutinas_db(atletismo,avanzado,quebradurabrazo,
	'Estirar por unos 5 minutos las piernas antes de iniciar. Luego para calentar da 2 vueltas trotando a una pista o por 1600m para calentar.
    Para la rutina realizar sprints de 100m con un descanso activo de 200m hasta completar 9km (un total de 30 veces). En la ultima ronda realizala
    al 100% de la velocidad posible y en los ultimo 200m disminuir la velocidad gradualmente hasta detenerse.').

rutinas_db(atletismo,avanzado,neumonia,
	'Estirar por unos 5 minutos las piernas antes de iniciar. Luego para calentar da 2 vueltas trotando a un paso moderado en una pista o 
    por 1600m para calentar.
    Para la rutina realiza sprints de 100m  con un descanso activo de 200m caminando hasta completar 9km (un total de 30 veces, cuida tu respiracion,
    reducelo a 15 vueltas en caso de sentirte mal). En la ultima ronda realizala con bastante velocidad pero cuidando de no hiperventilarte y en los
    ultimo 200m disminuir la velocidad gradualmente hasta detenerse.').

rutinas_db(atletismo,avanzado,asmatico,
	'Estirar por unos 5 minutos las piernas antes de iniciar. Luego para calentar da 2 vueltas trotando a un paso moderado en una pista o 
    por 1600m para calentar.
    Para la rutina realiza sprints de 100m  con un descanso activo de 200m caminando hasta completar 9km (un total de 30 veces, cuida tu respiracion,
    reducelo a 15 vueltas en caso de sentirte mal). En la ultima ronda realizala con bastante velocidad pero cuidando de no hiperventilarte y en los
    ultimo 200m disminuir la velocidad gradualmente hasta detenerse.').

rutinas_db(atletismo,avanzado,quebradurapierna,
	'No te recomiendo atletismo con una pierna rota, pero podrias hacer esta rutina: 
    Estirar por unos 5 minutos las piernas antes de iniciar. Luego camina dandole 1 vuelta a la pista para calentar. Luego has una pausa de unos
    segundos y repite esto unas 10 veces tratando en lo posible de ir con un paso un poco mas acelerado pero sin correr ni trotar para que almenos
    mantengas la fuerza en la pierna sana.').





%rutinas principiante crossfit
rutinas_db(crossfit,principiante, bien, 
	'Estirar los brazos y piernas por unos 5 minutos y trotar unos 500m para calentar. Luego para calentar realizar dominadas negativas, brincar a la barra y descender
    lentamente, 5 series de 3 repeticiones (5x3), luego en un tabta de 20 segundos activos y 10 de descanso, realizar cada ventana activa plancha y escaladores, repitiendo
    un total de 12 veces (6 minutos). Para la rutina, seran la mayor cantidad de repeticiones posibles (AMRAP) en 20 minutos, donde una ronda consiste en 10 dominadas, 10 lagartijas
    10 toques a la barra con los pies y 200 metros de corrida. Al terminar, recuerda estirar y estar hidratandote bien.').

rutinas_db(crossfit,principiante, quebradurapierna, 
	'No recomiendo realizar crossfit con una quebradura en una pierna, pero puedo darte una rutina para entrenar otras partes del cuerpo:
    Primero calienta con un tabata donde cada ventana activa de 20 segundos alterna entre abdominales y lagartijas con las rodillas en el
    piso, puedes colocar una almohadilla en las rodillas de ser necesario, esto por 6 minutos. Luego para la rutina, realiza un AMRAP (la 
    mayor cantidad de repeticiones por tiempo) en 20 minutos, donde una ronda consiste de 10 lagartijas, 10 abdominales y 5 dips en aros.').

rutinas_db(crossfit,principiante, quebradurabrazo, 
	'No recomiendo realizar crossfit con una quebradura en un brazo, pero puedo darte una rutina para entrenar otras partes del cuerpo:
    Primero estira bien las piernas y el torzo por 5 minutos. Luego para calentar realiza un EMOM (cada minuto un ejercicio) de 10 minutos
    de sentadillas y abdominales, la mayor cantidad posible en cada ventana, alternando. Luego para la rutina, en 20 minutos completa 5 rondas
    de correr 500m, 20 sentadillas y 10 thrusters con mancuerna de 25lb con el brazo sano').


rutinas_db(crossfit,principiante, asmatico,
	'Durante la rutina, manten un ritmo constante y moderado, no te aceleres y toma pausas para respirar. Estira los brazos y piernas por unos 5 minutos y trotar unos 500m para calentar. 
    Luego para calentar realizar dominadas negativas, brincar a la barra y descender lentamente, 5 series de 3 repeticiones (5x3), luego en un tabta de 20 segundos activos y 10 de descanso,
    realizar cada ventana activa plancha y escaladores, repitiendo un total de 12 veces (6 minutos). Para la rutina, seran la mayor cantidad de repeticiones posibles (AMRAP) en 15 minutos, 
    donde una ronda consiste en 5 dominadas, 10 lagartijas 5 toques a la barra con los pies y 200 metros de corrida. Al terminar, recuerda estirar y estar hidratandote bien.').

rutinas_db(crossfit,principiante, neumonia,
	'Durante la rutina, manten un ritmo constante y moderado, no te aceleres y toma pausas para respirar. Estira los brazos y piernas por unos 5 minutos y trotar unos 500m para calentar. 
    Luego para calentar realizar dominadas negativas, brincar a la barra y descender lentamente, 5 series de 3 repeticiones (5x3), luego en un tabata de 20 segundos activos y 10 de descanso,
    realizar cada ventana activa plancha y escaladores, repitiendo un total de 12 veces (6 minutos). Para la rutina, seran la mayor cantidad de repeticiones posibles (AMRAP) en 15 minutos, 
    donde una ronda consiste en 5 dominadas, 10 lagartijas 5 toques a la barra con los pies y 200 metros de corrida. Al terminar, recuerda estirar y estar hidratandote bien.').

rutinas_db(crossfit,principiante, hipertension,
	'Durante la rutina, manten un ritmo constante y moderado, no pongas mucha intensidad. Estira los brazos y piernas por unos 5 minutos y trotar unos 500m para calentar. 
    Luego para calentar realizar dominadas negativas, brincar a la barra y descender lentamente, 5 series de 3 repeticiones (5x3), luego en un tabata de 20 segundos activos y 10 de descanso,
    realizar cada ventana activa plancha y escaladores, repitiendo un total de 12 veces (6 minutos) a un ritmo moderado, no le metas intensidad. Para la rutina, seran la mayor cantidad de 
    repeticiones posibles (AMRAP) en 20 minutos, donde una ronda consiste en 5 dominadas, 10 lagartijas 5 toques a la barra con los pies y 100 metros de corrida. Al terminar, recuerda 
    estirar y estar hidratandote bien y no corras mucho').

rutinas_db(crossfit,principiante, hernia, 
	'Con una hernia, te recomiendo no hacer mucho esfuerzo. Primero estira los brazos y piernas por unos 5 minutos y trota unos 500m a un paso moderado para calentar. Luego 
    para calentar realiza lagartijas con las rodillas en el piso, 5 series de 5 repeticiones (5x5), luego en un tabata de 20 segundos activos y 10 de descanso, realizar cada 
    ventana activa abdominales y escaladores, repitiendo un total de 12 veces (6 minutos). Para la rutina, seran la mayor cantidad de repeticiones posibles (AMRAP) en 20 minutos,
    donde una ronda consiste en 10 abdominales, 10 lagartijas con rodillas en el piso, 10 v-ups y 200 metros de corrida. Al terminar, recuerda estirar y estar hidratandote bien.').

%rutinas intermedio crossfit
rutinas_db(crossfit,intermedio, bien,
	'Estirar los brazos y piernas por unos 5 minutos y trotar unos 800m para calentar. Primero calentaras con 5 rondas de burgener para arranque, agarrando la barra con agarre abierto e iniciando
    desde la altura de la cadera, realiza 3 jalones, 3 jalones hasta el pecho y 3 arranques completos que consiste en la combinacion de los movimientos anteriores, siempre manteniendo la
    barra bien pegada al cuerpo, hasta llevarla a la altura arriba de la cabeza. Luego realiza por 10 minutos la mayor cantidad de rondas posibles de 5 dominadas hasta el pecho, 5 toques a la barra con los pies
    y 30 segundos en plancha. Para la rutina, realizaras tenras 20 minutos para completar 10 repeticiones de 5 arranques a un 60% de su capacidad de peso, 15 toques de los pies a la barra y 10 saltos al cajon en la altura intermedia.
    Al finalizar, recuerda estirar completamente e hidratarte bien.').

rutinas_db(crossfit,intermedio, quebradurapierna, 
	'No recomiendo realizar crossfit con una quebradura en una pierna, pero puedo darte una rutina para entrenar otras partes del cuerpo:
    Primero calienta con un tabata donde cada ventana activa de 20 segundos alterna entre abdominales y lagartijas con las rodillas en el
    piso, puedes colocar una almohadilla en las rodillas de ser necesario, esto por 6 minutos. Luego para la rutina, realiza un AMRAP (la 
    mayor cantidad de repeticiones por tiempo) en 20 minutos, donde una ronda consiste de 10 lagartijas, 10 abdominales y 5 dips en aros.').

rutinas_db(crossfit,intermedio, quebradurabrazo, 
	'No recomiendo realizar crossfit con una quebradura en un brazo, pero puedo darte una rutina para entrenar otras partes del cuerpo:
    Primero estira bien las piernas y el torzo por 5 minutos. Luego para calentar realiza un EMOM (cada minuto un ejercicio) de 10 minutos
    de sentadillas y abdominales, la mayor cantidad posible en cada ventana, alternando. Luego para la rutina, en 20 minutos completa 5 rondas
    de correr 500m, 25 sentadillas y 10 thrusters con mancuerna de 35lb con el brazo sano').

rutinas_db(crossfit,intermedio, asmatico, 
	'Recomiendo que al realizar esta rutina, la realices a un paso moderado y tomando descansos para respirar de vez en cuando para que evites alguna complicacion
    Primero, vas a estirar los brazos y piernas por unos 5 minutos y trotar unos 500m para calentar. Luego calentaras con 5 rondas de burgener para arranque, agarrando la barra con agarre abierto e iniciando
    desde la altura de la cadera, realiza 3 jalones, 3 jalones hasta el pecho y 3 arranques completos que consiste en la combinacion de los movimientos anteriores, siempre manteniendo la
    barra bien pegada al cuerpo, hasta llevarla a la altura arriba de la cabeza. Luego realiza por 10 minutos la mayor cantidad de rondas posibles de 5 dominadas hasta el pecho, 5 toques a la barra con los pies
    y 30 segundos en plancha. Para la rutina, realizaras tenras 15 minutos para completar 5 repeticiones de 5 arranques a un 45% de su capacidad de peso, 15 toques de los pies a la barra y 5 saltos al cajon en la altura intermedia
    (puedes caminar los saltos para evitar complicaciones). Al finalizar, recuerda estirar completamente e hidratarte bien.').

rutinas_db(crossfit,intermedio, neumonia, 
	'Recomiendo que al realizar esta rutina, la realices a un paso moderado y tomando descansos para respirar de vez en cuando para que evites alguna complicacion
    Primero, vas a estirar los brazos y piernas por unos 5 minutos y trotar unos 500m para calentar. Luego calentaras con 5 rondas de burgener para arranque, agarrando la barra con agarre abierto e iniciando
    desde la altura de la cadera, realiza 3 jalones, 3 jalones hasta el pecho y 3 arranques completos que consiste en la combinacion de los movimientos anteriores, siempre manteniendo la
    barra bien pegada al cuerpo, hasta llevarla a la altura arriba de la cabeza. Luego realiza por 10 minutos la mayor cantidad de rondas posibles de 5 dominadas hasta el pecho, 5 toques a la barra con los pies
    y 30 segundos en plancha. Para la rutina, realizaras tenras 15 minutos para completar 5 repeticiones de 5 arranques a un 45% de su capacidad de peso, 15 toques de los pies a la barra y 5 saltos al cajon en la altura intermedia
    (puedes caminar los saltos para evitar complicaciones). Al finalizar, recuerda estirar completamente e hidratarte bien.').

rutinas_db(crossfit,intermedio, hipertension,
	'Estirar los brazos y piernas por unos 5 minutos y trotar a un paso moderado unos 400m para calentar. Primero calentaras con 5 rondas de burgener para arranque, agarrando la barra con agarre abierto e iniciando
    desde la altura de la cadera, realiza 3 jalones, 3 jalones hasta el pecho y 3 arranques completos que consiste en la combinacion de los movimientos anteriores, siempre manteniendo la
    barra bien pegada al cuerpo, hasta llevarla a la altura arriba de la cabeza. Luego realiza por 10 minutos la mayor cantidad de rondas posibles de 5 dominadas hasta el pecho, 5 toques a la barra con los pies
    y 30 segundos en plancha (descansa a ratos almenos a la mitad). Para la rutina, tendras 20 minutos para completar 10 repeticiones de 5 arranques a un 30% de su capacidad de peso, 9 toques de los pies a la barra y 10 subidas
    al cajon caminando en la altura intermedia. Al finalizar, recuerda estirar completamente e hidratarte bien.').

rutinas_db(crossfit,intermedio, hernia,
	'Estirar los brazos y piernas por unos 5 minutos y trotar a un paso moderado unos 400m para calentar. Primero calentaras con 5 rondas de burgener para arranque, agarrando la barra con agarre abierto e iniciando
    desde la altura de la cadera, realiza 1 jalon, 1 jalon hasta el pecho y 1 arranque completo que consiste en la combinacion de los movimientos anteriores, siempre manteniendo la
    barra bien pegada al cuerpo, hasta llevarla a la altura arriba de la cabeza. Luego realiza por 10 minutos la mayor cantidad de rondas posibles de 5 dominadas hasta el pecho, 5 sentadillas
    y 30 segundos de sentadilla isometrica(apoyate en la pared para evitar sobresfuerzo). Para la rutina, tendras 20 minutos para completar 10 repeticiones de 5 arranques con solo la barra,
    9 toques de los pies a la barra y 10 subidas al cajon caminando en la altura intermedia. Al finalizar, recuerda estirar completamente e hidratarte bien.').

%rutinas avanzado crossfit
rutinas_db(crossfit,avanzado,bien,
	'Estirar los brazos y piernas por unos 5 minutos y trotar 1km para calentar. Para calentar tendras 10 minutos para realizar la mayor cantidad de rondas de 20 saltos dobles y 10 saltos al cajon en la 
    altura intermedia. Luego 5 rondas de 1 arranque completo, 1 arranque con sentadilla y 1 arranque en posicion elevada. Para la rutina realiza un EMOM donde cada minuto debes completar 2 arranques con sentadilla
    con un peso de 145lb y 10 saltos al cajon en la altura mas alta. La duracion sera de 10 minutos para un total de 10 rondas, en caso de no terminar una antes que termine el tiempo, debes dejarla donde quedo
    e iniciar con los 2 arranques de una vez. Al finalizar estira completamente y recuerda hidratarte bien.').

rutinas_db(crossfit,avanzado, quebradurapierna, 
	'No recomiendo realizar crossfit con una quebradura en una pierna, pero puedo darte una rutina para entrenar otras partes del cuerpo:
    Primero calienta con un tabata donde cada ventana activa de 20 segundos alterna entre abdominales y lagartijas con las rodillas en el
    piso, puedes colocar una almohadilla en las rodillas de ser necesario, esto por 6 minutos. Luego para la rutina, realiza un AMRAP (la 
    mayor cantidad de repeticiones por tiempo) en 20 minutos, donde una ronda consiste de 20 lagartijas, 15 abdominales y 10 dips en aros.').

rutinas_db(crossfit,avanzado, quebradurabrazo, 
	'No recomiendo realizar crossfit con una quebradura en un brazo, pero puedo darte una rutina para entrenar otras partes del cuerpo:
    Primero estira bien las piernas y el torzo por 5 minutos. Luego para calentar realiza un EMOM (cada minuto un ejercicio) de 10 minutos
    de sentadillas y abdominales, la mayor cantidad posible en cada ventana, alternando. Luego para la rutina, en 20 minutos completa 10 rondas
    de correr 500m, 20 sentadillas y 10 thrusters con mancuerna de 55lb con el brazo sano').

rutinas_db(crossfit,avanzado, asmatico,
	'Manten un ritmo constante y con algunas pausas para respirar ya que te podrias hiperventilar con facilidad. Primero vas a estirar los brazos 
    y piernas por unos 5 minutos y luego trota 500m para calentar. Para calentar tendras 10 minutos para realizar la mayor cantidad de rondas de 
    10 saltos dobles y 5 saltos al cajon en la altura intermedia. Luego 5 rondas de 1 arranque completo, 1 arranque con sentadilla y 1 arranque en 
    posicion elevada. Para la rutina realiza un EMOM donde cada minuto debes completar 2 arranques con sentadilla con un peso de 95lb y 5 saltos 
    al cajon en la altura intermedia. La duracion sera de 10 minutos para un total de 10 rondas, en caso de no terminar una antes que termine el tiempo, 
    debes dejarla donde quedo e iniciar con los 2 arranques de una vez. Al finalizar estira completamente y recuerda hidratarte bien.').

rutinas_db(crossfit,avanzado, neumonia, 
	'Manten un ritmo constante y con algunas pausas para respirar ya que te podrias hiperventilar con facilidad. Primero vas a estirar los brazos 
    y piernas por unos 5 minutos y luego trota 500m para calentar. Para calentar tendras 10 minutos para realizar la mayor cantidad de rondas de 
    10 saltos dobles y 5 saltos al cajon en la altura intermedia. Luego 5 rondas de 1 arranque completo, 1 arranque con sentadilla y 1 arranque en 
    posicion elevada. Para la rutina realiza un EMOM donde cada minuto debes completar 2 arranques con sentadilla con un peso de 95lb y 5 saltos 
    al cajon en la altura intermedia. La duracion sera de 10 minutos para un total de 10 rondas, en caso de no terminar una antes que termine el tiempo, 
    debes dejarla donde quedo e iniciar con los 2 arranques de una vez. Al finalizar estira completamente y recuerda hidratarte bien.').

rutinas_db(crossfit,avanzado, hipertension, 
	'Estirar los brazos y piernas por unos 5 minutos y trotar 500m para calentar a un ritmo moderado. Para calentar tendras 10 minutos para realizar 
    la mayor cantidad de rondas de 10 saltos dobles y 10 subidas caminando al cajon en la altura intermedia. Luego 5 rondas de 1 arranque completo, 
    1 arranque con sentadilla y 1 arranque en posicion elevada con solo la barra. Para la rutina realiza un EMOM donde cada minuto debes completar 
    2 arranques con sentadilla con un peso de 95lb y 3 saltos al cajon en la altura mas alta. La duracion sera de 10 minutos para un total de 10 
    rondas, en caso de no terminar una antes que termine el tiempo, debes dejarla donde quedo e iniciar con los 2 arranques de una vez. Al finalizar estira completamente y recuerda hidratarte bien.').

rutinas_db(crossfit,avanzado, hernia, 
	'No cargues peso en la rutina que te voy a dar, tampoco hagas mucha fuerza, si necesitas descansar, detente un momento para evitar empeorar. Estirar 
    los brazos y piernas por unos 5 minutos y trotar 400m para calentar. Para calentar tendras 10 minutos para realizar la mayor cantidad de rondas de 10
    saltos sencillos a la cuerda y 10 subidas caminando al cajon en la altura mas baja. Luego 5 rondas de 1 arranque completo y 1 arranque con sentadilla,
    haz una pausa entre cada ronda. Para la rutina realiza un EMOM donde cada minuto debes completar 2 arranques con sentadilla con unicamente la barra y 
    5 subidas caminando al cajon en la altura intermedia. La duracion sera de 10 minutos para un total de 10 rondas, en caso de no terminar una antes que 
    termine el tiempo, debes dejarla donde quedo e iniciar con los 2 arranques de una vez, recuerda realizarlos lentamente y sin hacer mucha fuerza. Al 
    finalizar estira completamente y recuerda hidratarte bien.').

%rutinas principiante halterofilia
rutinas_db(halterofilia, principiante,bien, 
	'Para calentar iniciaremos con solo la barra haciendo 5 jalones, 5 jalones al pecho y 5 cleans que seria combinando los movimientos anteriores y llevando la barra hasta los hombros, realiza esto 15 veces. Luego con solo la barra 
    realiza 20 cleans pausados, uno por uno, para familiarizarte con el ejercicio, siempre manteniendo la barra bien pegada al cuerpo. Luego, realiza 10 rondas de 5 lagartijas 10 abdominales y 15 sentadillas, para in construyendo fuerza.
    Y para finalizar, con un disco de 10kg a cada lado, realiza 30 repeticiones de clean, uno por uno, a un paso moderado, soltando la barra cada vez.').

rutinas_db(halterofilia, principiante,asmatico, 
	'Para calentar iniciaremos con solo la barra haciendo 5 jalones, 5 jalones al pecho y 5 cleans que seria combinando los movimientos anteriores y llevando 
    la barra hasta los hombros, realiza esto 15 veces. Luego con solo la barra realiza 20 cleans pausados, uno por uno, para familiarizarte con el ejercicio, 
    (recuerda descannsar un intervalo de tiempo entre cada uno, almenos 5 segundos, no te agites) siempre manteniendo la barra bien pegada al cuerpo. Luego, 
    realiza 10 rondas de 5 lagartijas 10 abdominales y 15 sentadillas, para in construyendo fuerza. Para finalizar, con un disco de 10kg a cada lado, realiza 
    15 repeticiones de clean, uno por uno, a un paso moderado, soltando la barra cada vez.').

rutinas_db(halterofilia, principiante,neumonia, 
	'Para calentar iniciaremos con solo la barra haciendo 5 jalones, 5 jalones al pecho y 5 cleans que seria combinando los movimientos anteriores y llevando 
    la barra hasta los hombros, realiza esto 15 veces. Luego con solo la barra realiza 20 cleans pausados, uno por uno, para familiarizarte con el ejercicio, 
    (recuerda descannsar un intervalo de tiempo entre cada uno, almenos 5 segundos, no te agites) siempre manteniendo la barra bien pegada al cuerpo. Luego, 
    realiza 10 rondas de 5 lagartijas 10 abdominales y 15 sentadillas, para in construyendo fuerza. Para finalizar, con un disco de 10kg a cada lado, realiza 
    15 repeticiones de clean, uno por uno, a un paso moderado, soltando la barra cada vez.').

rutinas_db(halterofilia, principiante, hipertension,
	'Para calentar iniciaremos con solo la barra haciendo 5 jalones, 5 jalones al pecho y 5 cleans que seria combinando los movimientos anteriores y llevando 
    la barra hasta los hombros, realiza esto 8 veces y pausando entre cada movimiento, no te agites. Luego con solo la barra realiza 20 cleans pausados, uno 
    por uno, para familiarizarte con el ejercicio(recuerda descannsar un intervalo de tiempo entre cada uno, almenos 5 segundos, no te agites), siempre 
    manteniendo la barra bien pegada al cuerpo. Luego, realiza 5 rondas de 5 lagartijas 10 abdominales y 15 sentadillas, para in construyendo fuerza. 
    Para finalizar, con un disco de 5kg a cada lado, realiza 10 repeticiones de clean, uno por uno, a un paso moderado, soltando la barra cada vez.').

rutinas_db(halterofilia, principiante, quebradurapierna,
	'No recomiendo hacer halterofilia con un brazo quebrado, pero te recomiendo para mantener la fuerza y condicion, que realices 15 series de 5 sentadillas y 5
    abdominales').

rutinas_db(halterofilia, principiante, quebradurabrazo,
	'No recomiendo hacer halterofilia con un brazo quebrado, pero te recomiendo para mantener la fuerza y condicion, que realices 15 series de 5 sentadillas y 5
    abdominales').

rutinas_db(halterofilia, principiante, hernia,
	'No recomiendo realizar halterofilia con una hernia ya que es un ejercicio de mucho peso. Si buscas mantener la condicion, puedes salir a trotar a un paso
    lento por una media hora la distancia que alcances.').

%rutinas intermedio halterofilia
rutinas_db(halterofilia, intermedio,bien, 
	'Para calentar iniciaremos con 30 repeticiones de arranque usando solo la barra. Una vez hecho esto, completa 20 repeticiones de peso muerto con 20kg a cada lado, de forma moderada, sin apuro. Una vez terminado,
    descansa por unos 5 minutos, caminando sin sentarte y luego carga un 50% del peso maximo de arranque y realiza 30 repeticiones a un paso moderado. Recuerda respirar y al finalizar estira bien el cuerpo.').

rutinas_db(halterofilia, intermedio,neumonia, 
	'Para calentar iniciaremos con 20 repeticiones de arranque usando solo la barra. Una vez hecho esto, completa 15 repeticiones de peso muerto con 10kg a cada lado,
    de forma moderada, sin apuro y respirando entre cada repeticion. Una vez terminado, descansa por unos 5 minutos, caminando sin sentarte y luego carga un 40% del 
    peso maximo de arranque y realiza 20 repeticiones a un paso moderado. Recuerda respirar entre cada repeticion para evitar cualquier inconveniente con tu condicion 
    y al finalizar estira bien el cuerpo.').

rutinas_db(halterofilia, intermedio,asmatico, 
	'Para calentar iniciaremos con 20 repeticiones de arranque usando solo la barra. Una vez hecho esto, completa 15 repeticiones de peso muerto con 10kg a cada lado,
    de forma moderada, sin apuro y respirando entre cada repeticion con un descanso. Una vez terminado, descansa por unos 5 minutos, caminando sin sentarte y luego carga un 40% del 
    peso maximo de arranque y realiza 20 repeticiones a un paso moderado. Recuerda respirar entre cada repeticion para evitar cualquier inconveniente con tu condicion 
    y al finalizar estira bien el cuerpo.').

rutinas_db(halterofilia, intermedio, hipertension,
	'Para calentar iniciaremos con 20 repeticiones de arranque usando solo la barra. Una vez hecho esto, completa 15 repeticiones de peso muerto con 10kg a cada lado,
    de forma moderada, sin apuro y respirando entre cada repeticion. Una vez terminado, descansa por unos 5 minutos, caminando sin sentarte y luego carga un 20% del 
    peso maximo de arranque y realiza 15 repeticiones a un paso moderado. Recuerda descansar entre cada repeticion para evitar cualquier inconveniente con tu condicion 
    y al finalizar estira bien el cuerpo.').

rutinas_db(halterofilia, intermedio, quebradurapierna,
	'No recomiendo hacer halterofilia con un brazo quebrado, pero te recomiendo para mantener la fuerza y condicion, que realices 15 series de 5 sentadillas y 5
    abdominales').

rutinas_db(halterofilia, intermedio, quebradurabrazo,
	'No recomiendo hacer halterofilia con un brazo quebrado, pero te recomiendo para mantener la fuerza y condicion, que realices 15 series de 5 sentadillas y 5
    abdominales').

rutinas_db(halterofilia, intermedio, hernia,
	'No recomiendo realizar halterofilia con una hernia ya que es un ejercicio de mucho peso. Si buscas mantener la condicion, puedes salir a trotar a un paso
    lento por una media hora la distancia que alcances.').

%rutinas avanzado halterofilia
rutinas_db(halterofilia, avanzado,bien, 
	'Para calentar inicia trotando unos 500m, luego con la barra, realiza 10 rondas de burgener para clean para ir calentando el movimiento. Luego realiza 10 rondas de 5 repeticiones de 
    levantamientos arriba de la cabeza, lentamente para ir calentando el movimiento. Por ultimo, realiza 40 repeticiones de levantamientos clean y jerk con un peso del 50% de tu RM en el movimiento de jerk.').

rutinas_db(halterofilia, avanzado,neumonia, 
	'Para calentar inicia trotando unos 500m a un paso moderado, luego con la barra, realiza 10 rondas de burgener para clean para ir calentando el movimiento
    pero recuerda pausar entre cada ronda almenos unos 30 segundos para no hiperventilarte. Luego realiza 10 rondas de 5 repeticiones de levantamientos arriba
    de la cabeza, lentamente para ir calentando el movimiento y recuerda hacerlo pausado. Por ultimo, realiza 25 repeticiones de levantamientos clean y jerk con 
    un peso del 50% de tu RM en el movimiento de jerk.').

rutinas_db(halterofilia, avanzado,asmatico, 
	'Para calentar inicia trotando unos 500m a un paso moderado, luego con la barra, realiza 10 rondas de burgener para clean para ir calentando el movimiento
    pero recuerda pausar entre cada ronda almenos unos 30 segundos para no hiperventilarte. Luego realiza 10 rondas de 5 repeticiones de levantamientos arriba
    de la cabeza, lentamente para ir calentando el movimiento y recuerda hacerlo pausado. Por ultimo, realiza 25 repeticiones de levantamientos clean y jerk con 
    un peso del 50% de tu RM en el movimiento de jerk.').

rutinas_db(halterofilia, avanzado, hipertension,
	'Para calentar inicia trotando unos 400m a un paso moderado, luego con la barra, realiza 10 rondas de burgener para clean para ir calentando el movimiento
    pero recuerda pausar entre cada ronda almenos unos 30 segundos para no agitarte. Luego realiza 15 rondas de 2 repeticiones de levantamientos arriba
    de la cabeza, lentamente para ir calentando el movimiento y recuerda hacerlo pausado, no te agites. Por ultimo, realiza 20 repeticiones de levantamientos clean y jerk con 
    un peso del 50% de tu RM en el movimiento de jerk a un ritmo moderado, descansa entre cada repeticion y evita agitarte.').

rutinas_db(halterofilia, avanzado, quebradurapierna,
	'No recomiendo hacer halterofilia con un brazo quebrado, pero te recomiendo para mantener la fuerza y condicion, que realices 25 series de 5 sentadillas y 5
    abdominales').

rutinas_db(halterofilia, avanzado, quebradurabrazo,
	'No recomiendo hacer halterofilia con un brazo quebrado, pero te recomiendo para mantener la fuerza y condicion, que realices 25 series de 5 sentadillas y 5
    abdominales').

rutinas_db(halterofilia, avanzado, hernia,
	'No recomiendo realizar halterofilia con una hernia ya que es un ejercicio de mucho peso. Si buscas mantener la condicion, puedes salir a trotar a un paso
    lento por una hora o hora y media la distancia que alcances.').


padecimientos(['hernia','fractura en brazos','fractura en piernas','hipertension','asmatico','neumonia']).


?-mrtrainer. % Ejecuta el programa
