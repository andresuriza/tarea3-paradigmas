:- use_module(library(random)).

% Regla principal que llama mensaje de bienvenida y procesa la
% conversación
mrtrainer:-
	%print_welcome,
	%flush_output,
	conversations.

% Regla que crea un loop para procesar la conversación hasta que el
% usuario se despida
conversations:-
	repeat,
	print_prompt(you),
	readin(S),
	gen_reply(S,R),
	print_prompt(me),
	write_list(R),
	is_quit(S), !, sleep(3), halt.

my_icon('MrTrainer').
user_icon('Cliente').

% Regla que imprime mensaje de lista en terminal
write_list([]):- nl.
write_list([H|T]):- write(H), write(' '), write_list(T).

% Regla que llama al nombre del usuario chat para simular interfaz de
% chat
print_prompt(me):-
        my_icon(X), write(X), write(': '), flush_output.

% Regla que llama al nombre del usuario que utiliza el programa para
% simular interfaz de chat
print_prompt(you):-
        user_icon(X), write(X), write(': '), flush_output.

% Regla que identifica el mensaje del usuario
readin(S):- read_in(L), my_filter(L,S).

% REVISAR
read_in(P):- initread(L), words(P, L, []).

% Lee el primer caracter ingresado por el usuario, llama a leer el
% resto de la lista
initread([K1,K2|U]):- get_code(K1),get_code(K2),readrest(K2,U).

% ------ REVISAR --------------
%  Crea atoms?
my_filter([],[]).
my_filter(['\n'|T], R):- !, my_filter(T, R).
my_filter([nb(2), X|T], [Rm|R]):-
        name(X, CharList),
        q_followed_by_nb(CharList),!,
        name(Rm, [50 | CharList]),
        my_filter(T, R).
my_filter([X|T], [X|R]):-
        my_filter(T, R).

% Si se recibe oración, procesar palabras recursivamente
words([V|U]) --> word(V),!, blanks, words(U).
% Si no hay nada, regresa vacío
words([]) --> [].

% Procesa palabra en ASCII,
word(U1) --> [K],{lc(K,K1)},!,alphanums(U2),{name(U1,[K1|U2])}.
word(nb(N)) --> [K],{digit(K)},!,digits(U),{name(N,[K|U])}.
word(V) --> [K],{name(V,[K])}.

% Genera recursivamente caracteres alfanuméricos en base a los códigos
% recibidos
alphanums([K1|U]) --> [K],{alphanum(K,K1)},!,alphanums(U).
alphanums([]) --> [].

% REVISAR
alphanum(95,95) :- !.
alphanum(K,K1):- lc(K,K1).
% REVISAR
alphanum(K,K):- digit(K).

% Si es un espacio o otra señal desconocida
blanks --> [K], {K=<32},!, blanks.
% De lo contrario, blanks son vacíos
blanks --> [].

% REVISAR
q_followed_by_nb([113, X|_]):- digit(X).

% Si el caracter es un número, se retorna
digits([K|U]) --> [K],{digit(K)},!,digits(U).
% Si es vacío se regresa lista vacía
digits([]) --> [].

% Verifica si es un número
digit(K):- K>47, K<58.

% Procesa letras en ASCII, mayúsculas y minúsculas
lc(K,K1):- K>64,K<91,!,K1 is K+32.
lc(K,K):- K>96,K<123.

% Respuesta si mrtrainer no entiende entrada
%gen_reply(S, R):-
%	\+ is_question(S),
%	\+ is_quit(S),
%	\+ is_thanks(S),
%	\+ is_greeting(S), !,
%	responses_db(random_s, Res),
%	random_pick(Res, R).

% Respuesta si usuario dijo algún tipo de saludo
gen_reply(S, R):-
	is_greeting(S), !,
	responses_db(greeting, Res),
	random_pick(Res, R).

% Respuesta si usuario dijo algún tipo de gracias
gen_reply(S, R):-
	is_thanks(S), !,
	responses_db(gracias, Res),
	random_pick(Res, R).

% Respuesta si usuario dijo algún tipo de despedida
gen_reply(S, R):-
	is_quit(S), !,
	responses_db(bye, Res),
	random_pick(Res, R).

gen_reply(S, R):-
	question(Tree2, S, _Rest), 
	mapping(s2name,Tree1, Tree2), !,
	sentence(Tree1, Rep,[]),
	append(Rep, ['!'], R).

gen_reply(S, R):-
	question(Tree2, S, _Rest), !, 
	mapping(s2how,Tree1, Tree2),
	sentence(Tree1, Rep,[]), !,
	append(Rep, ['!'], R).

gen_reply(S, R):-
	sentence(Tree1, S, _Rest), !, 
	mapping(s2why,Tree1, Tree2), % Respuesta de mrtrainer
	question(Tree2, Rep,[]),
	append(Rep, ['?'], R).

gen_reply(S, R):-
	question(Tree2, S, _Rest), !, 
	mapping(s2q,Tree1, Tree2),
	sentence(Tree1, Rep,[]),
	append([yes, ','|Rep], ['!'], R).

% Regla que regresa a partir de una lista el valor del índice
% especificado
nth_item([H|_], 1, H).
nth_item([_|T], N, X):-
	nth_item(T, N1, X),
	N is N1 + 1.

% Regla que escoge un valor aleatorio de una lista y lo retorna
random_pick(Res, R):-
	length(Res, Length),
	Upper is Length + 1,
	random(1, Upper, Rand),
	nth_item(Res, Rand, R).

% Llama recursivamente la palabra para leer sus letras
readrest(63,[]):- !.
readrest(33,[]):- !.
readrest(10,[]):- !.
readrest(K,[K1|U]):- K=<32,!,get_code(K1),readrest(K1,U).
readrest(_K1,[K2|U]):- get_code(K2),readrest(K2,U).

% Regla que verifica si lista es miembro de otra lista
subset([], _).
subset([H|T], L2):-
	member(H, L2),
	subset(T, L2).

sentence(s(X,Y, is, Z)) --> belonging_phrase(X), abstract_noun(Y),  
                              [is],  special_noun(Z).

sentence(s(X, Y, Z)) --> subject_pronoun(X), indicative_verb(Y), 
                         adjective(Z).

sentence(s(X, Y, Z)) --> subject_phrase(X), verb(Y), object_phrase(Z).

sentence(s(X, Y, Z)) --> question(X), determiner(Y), place_name(Z).

sentence(s(X, Y)) --> determiner(X), place_name(Y).

sentence(s(X, Y)) --> subject_tobe_verb(X), prepositional_phrase(Y).

sentence(s(X, Y, Z)) --> question(X), object_pronoun(Y), noun(Z).

belonging_phrase(belong(your)) --> [your].
belonging_phrase(belong(my)) --> [my].

abstract_noun(abs_noun(name)) --> [name].

special_noun(sp_noun(justin)) --> [justin].
special_noun(sp_noun(frank)) --> [frank].

place_name(pname(reception)) --> [reception].
place_name(pname(cafe)) --> [cafe].
place_name(pname(toilet)) --> [toilet].
place_name(pname(vending_machines)) --> [vending_machines].
place_name(pname(lockers)) --> [lockers].
place_name(pname(exit)) --> [exit].
place_name(pname(london)) --> [london].
place_name(pname(bristol)) --> [bristol].
place_name(pname(exeter)) --> [exeter].
place_name(pname(X)) --> [X], { next(X,_,_,_,_) }.

subject_phrase(sp(X)) --> subject_pronoun(X).
subject_phrase(sp(X)) --> noun_phrase(X).

object_phrase(op(X,Y)) --> noun_phrase(X), adverb(Y).
object_phrase(op(X, Y)) --> object_pronoun(X), adverb(Y).

noun_phrase(np(X, Y)) --> determiner(X), noun(Y).
noun_phrase(np(Y)) --> noun(Y).

prepositional_phrase(pp(X, Y)) --> preposition(X), place_name(Y).

preposition(prep(in)) --> [in].
preposition(prep(at)) --> [at].
preposition(prep(from)) --> [from].


subject_pronoun(spn(i)) --> [i].
subject_pronoun(spn(we)) --> [we].
subject_pronoun(spn(you)) --> [you].
subject_pronoun(spn(they)) --> [they].
subject_pronoun(spn(he)) --> [he].
subject_pronoun(spn(she)) --> [she].
subject_pronoun(spn(it)) --> [it].
subject_pronoun(spn(who)) --> [who].

object_pronoun(opn(you))--> [you].
object_pronoun(opn(your))--> [your].
object_pronoun(opn(me))--> [me].
object_pronoun(opn(us))--> [us].
object_pronoun(opn(them))--> [them].
object_pronoun(opn(him))--> [him].
object_pronoun(opn(her))--> [her].
object_pronoun(opn(it))--> [it].

determiner(dtmnr([])) --> [].
determiner(dtmnr([a])) --> [a].
determiner(dtmnr([the])) --> [the].
determiner(dtmnr([my])) --> [my].
determiner(dtmnr([some])) --> [some].
determiner(dtmnr([all])) --> [all].
determiner(dtmnr([that])) --> [that].

noun(noun(uwe)) --> [uwe].
noun(noun(cs_course)) --> [cs_course].
noun(noun(robotics_course)) --> [robotics_course].
noun(noun(robotics_course)) --> [computing_course].
noun(noun(robotics_course)) --> [sd_course].
noun(noun(name)) --> [name].

adverb(ad([very, much])) --> [very, much].
adverb(ad([how])) --> [how].
adverb(ad([])) --> [].

verb(vb(like)) --> [like].
verb(vb(love)) --> [love].
verb(vb(is)) --> [is].

indicative_verb(ivb(are)) --> [are].
indicative_verb(ivb(am)) --> [am].

subject_tobe_verb(s_2b([you, are])) --> [you, are].
subject_tobe_verb(s_2b([i,am])) --> [i, am].
subject_tobe_verb(s_2b([we, are])) --> [we, are].

adjective(adj(great)) --> [great].
adjective(adj(good)) --> [good].
adjective(adj(fine)) --> [fine].

question(q(why,do,S)) --> [why, do], sentence(S).
question(q(do,S)) --> [do], sentence(S).

question(q(X, Y, Z)) --> adverb(X), indicative_verb(Y), subject_pronoun(Z).
question( q( what, is, X, Y ) ) -->  [what, is],  belonging_phrase(X),  
                                     abstract_noun(Y).   

mapping(s2why, 
        s(sp(spn(N1)),vb(V),op(opn(N2),ad(X))),
        q(why,do,s(sp(spn(P1)),vb(V),op(opn(P2),ad(X)))) 
        ) :- 
        mapping_spn(N1, P1), mapping_opn(N2, P2). 
mapping(s2why,
        s(sp(spn(N1)),vb(V),op(np(noun(N2)),ad(X))),
        q(why,do,s(sp(spn(P1)),vb(V),op(np(noun(N2)),ad(X)))) 
        ) :- 
        mapping_spn(N1, P1).


mapping(s2q,
        s(sp(spn(N1)),vb(V),op(opn(N2),ad(X))),
        q(do,s(sp(spn(P1)),vb(V),op(opn(P2),ad(X)))) 
        ) :- 
        mapping_spn(N1, P1), mapping_opn(N2, P2). 
mapping(s2q,
        s(sp(spn(N1)),vb(V),op(np(noun(N2)),ad(X))),
        q(do,s(sp(spn(P1)),vb(V),op(np(noun(N2)),ad(X)))) 
        ) :- 
        mapping_spn(N1, P1).

mapping(s2name,
        s( belong(Y1), abs_noun(X2), is, sp_noun(Y2) ),
        q( what, is, belong(X1), abs_noun(X2) )
        ):-
        mapping_belong(X1, Y1), mapping_noun(X2, Y2).

mapping(s2how,
        s(spn(X1), ivb(Y1), adj(_)),
        q(ad(_), ivb(Y2), spn(Z2))
        ):-
        mapping_spn(X1, Z2), mapping_indicative(Y1, Y2).

mapping_belong(my,your).
mapping_belong(your,my).

mapping_noun(name, frank).
mapping_noun(frank, name).

mapping_indicative(are, am).
mapping_indicative(am, are).

mapping_ad(how, fine).
mapping_ad(fine, how).

mapping_spn(i, you).
mapping_spn(you, i).

mapping_opn(you,me).
mapping_opn(me,you).

intersect([], _, []).
intersect([H|T1], L2, [H|T3]):-
        member(H, L2), !,
        intersect(T1, L2, T3).
intersect([_|T1], L2, L3):-
        intersect(T1, L2, L3).

% Regla que verifica si se mandó mensaje de despedida en la oración
is_quit(S):-
	despedida_db(D),
	intersect(S, D, A),
	A \== [].

% Regla que verifica si se mandó mensaje de gracias en la oración
is_thanks(S):-
	gracias_db(D),
	intersect(S, D, A),
	A \== [].

% Regla que verifica si hay signo de pregunta en la oración
is_question(S):-
	member('?', S).

% Regla que verifica si se mandó mensaje de saludo en la oración
is_greeting(S):-
	greeting_db(D),
	intersect(S, D, A),
	A \== [].

% Posibles palabras clave de saludo
greeting_db([
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
	solamente
	]).

% Posibles palabras clave de gracias
gracias_db([
	gracias,
	agradecido,
	agradecida
	]).

% Posibles mensajes de saludo de mrtrainer
responses_db(greeting, [
	['Cuénteme ¿En qué lo puedo ayudar?'],
	['Hola, listo para ayudar'],
	['Buenas, cómo está?'],
	['Hola soy Mr. Trainer, a su servicio']
	]).
	
% Posibles respuestas a gracias de mrtrainer
responses_db(gracias, [
	['Con mucho gusto'],
	['¡Con gusto! Cualquier otra consulta, estoy a tu servicio'],
	['El placer es mío']
	]).

% Posibles mensajes de despedida de mrtrainer
responses_db(bye, [
	['Espero que haya sido de ayuda, ¡suerte!'],
	['Que tengas buen día'],
	['¡Hasta luego!']
	]).

% Posibles respuestas de preguntas
responses_db(random_q, [
	['No estoy seguro, pregúntame algo distinto'],
	['No tengo conocimiento de eso, ¡disculpa!']
	]).

% Oraciones conectoras
responses_db(random_s, [
	['Disculpa, no entendí lo que dices'],
	['¿Podrías repetir eso?'],
	['No estoy seguro de entender eso']
	]).

% --- Eliminar -----------------------------
next('exit2', '2q56', east, right, 5).          
next('2q56', 'exit2', west, right, 5).          
next('exit1', 'area1', west, right, 2).
next('area1', 'exit1', east, right, 2).
next('exit2', 'exit1', west, right, 1).
next('exit1', 'exit2', east, right, 1).
next('area1', 'exit3', west, front, 2).
next('exit3', 'area1', east, left, 2).
next('2q56', '2q4', east, left, 3).
next('2q4', '2q56', west, left, 3).
next('junt2', 'junt1', west, front, 5).
next('junt1', 'junt2', east, right, 5).
next('2q4', 'junt1', east, front, 1).
next('junt1', '2q4', west, right, 1).
next('junt1', '2q5', north, left, 2).
next('2q5', 'junt1', south, front, 2).
next('2q5', '2q6', north, left, 5).
next('2q6', '2q5', south, right, 5).
next('2q6', '2q31', north, right, 4).
next('2q31', '2q6', south, right, 4).           
next('2q31', '2q30', north, right, 5).
next('2q30', '2q31', south, left, 5).
next('2q30', '2q9', north, left, 2).
next('2q9', '2q30', south, left, 2).            
next('2q9', '2q29', north, right, 2).
next('2q29', '2q9', south, right, 2).           
next('2q29', 'exit5', north, left, 1).
next('exit5', '2q29', south, left, 1).          
next('exit5', 'junt3', north, front, 12).
next('junt3', 'exit5', south, right, 12).
next('junt3', '2q23', north, right, 6).
next('2q23', 'junt3', south, front, 6).
next('2q23', '2q21', north, right, 1).
next('2q21', '2q23', south, left, 1).
next('2q21', 'exit6', north, front, 1).
next('exit6', '2q21', south, left, 1).
next('junt3', 'area2', east, right,10).
next('area2', 'junt3', west, front,10).
next('area2', '2q24', east, left,2).
next('2q24', 'area2', west, left,2).            
next('2q24', '2q25', east, left,4).
next('2q25', '2q24', west, right,4).
next('junt2','2q52', south, right,3).
next('2q52', 'junt2', north, front,3).
next('2q52', '2q50', south, right, 6).
next('2q50', '2q52', north, left, 6).
next('2q50', '2q42', south, left, 4).
next('2q42', '2q50', north, left, 4).           
next('2q42', '2q43', south, left, 2).
next('2q43', '2q42', north, right, 2).
next('2q43', '2q49', south, right, 1).
next('2q49', '2q43', north, right, 1).          
next('2q49', 'area3', south, left, 1).
next('area3', '2q49', north, left, 1).
next('area3', '2q46', south, left, 3).
next('2q46', 'area3', north, right, 3).
next('2q46', '2q47', south, left, 2).
next('2q47', '2q46', north, right, 2).
next('2q47', '2q48', south, right, 2).
next('2q48', '2q47', north, right, 2).
next('2q48', 'exit4', south, front, 2).
next('exit4', '2q48', north, left, 2).          

?-mrtrainer. % Ejecuta el programa