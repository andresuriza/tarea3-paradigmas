:- use_module(library(random)).

% Regla principal que llama mensaje de bienvenida y procesa la
% conversación
mrtrainer:-
	print_welcome,
	conversations.

% Regla que escoge aleatoriamente un mensaje de bienvenida
print_welcome:-
	responses_db(greeting, D),
	random_pick(D, W),
	print_prompt(me),
	write_list(W),
	flush_output.

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

gen_reply(S, R):-
	\+ is_question(S), \+ is_quit(S), !,
	responses_db(random_q, Res),
	random_pick(Res, R).

gen_reply(S, R):-
	is_question(S), !,
	responses_db(random_s, Res),
	random_pick(Res, R).

gen_reply(S, R):-
	is_quit(S), !,
	responses_db(bye, Res),
	random_pick(Res, R).

nth_item([H|_], 1, H).
nth_item([_|T], N, X):-
	nth_item(T, N1, X),
	N is N1 + 1.

random_pick(Res, R):-
	length(Res, Length),
	Upper is Length + 1,
	random(1, Upper, Rand),
	nth_item(Res, Rand, R).

readrest(63,[]):-!.
readrest(33,[]):-!.
readrest(10,[]):-!.
readrest(K,[K1|U]):- K=<32,!,get_code(K1),readrest(K1,U).
readrest(_K1,[K2|U]):- get_code(K2),readrest(K2,U).


subset([], _).
subset([H|T], L2):-
	member(H, L2),
	subset(T, L2).

is_quit(S):-
	subset([bye], S).
is_question(S):-
	member('?', S).

responses_db(greeting, [
	['Cuénteme ¿En qué lo puedo ayudar?'],
	['Hola, listo para ayudar'],
	['Buenas, cómo está?'],
	['Hola soy Mr. Trainer, a su servicio']
	]).

responses_db(bye, [
	['Espero que haya sido de ayuda, ¡suerte!'],
	['Que tengas buen día'],
	['¡Hasta luego!']
	]).

responses_db(random_q, [
	['No estoy seguro, pregúntame algo distinto'],
	['No tengo conocimiento de eso, ¡disculpa!']
	]).

responses_db(random_s, [
	['Disculpa, no entendí lo que dices'],
	['¿Podrías repetir eso?'],
	['No estoy seguro de entender eso']
	]).

write_list([]):- nl.
write_list([H|T]):- write(H), write(' '), write_list(T).

?-mrtrainer. % Ejecuta el programa
