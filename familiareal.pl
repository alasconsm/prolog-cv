:-initialization(main).
main:- genealogical_base.

genealogical_base:-
    write('-------Base de datos Genealogica: Familia Real-------'),nl,nl,
    write('Puedes formular preguntas como: '),nl,nl,
    tab(2),write('1. quien es el padre de <nombre>?'),nl,
    tab(2),write('2. quien es el abuelo de <nombre>?'),nl,
    tab(2),write('3. quien es la madre de <nombre>?'),nl,
    tab(2),write('4. quien es la abuela de <nombre>?'),nl,
    tab(2),write('5. quien es el abuelo de <nombre>?'),nl,
    tab(2),write('7. quien es la madre de <nombre>?'),nl,
    tab(2),write('9. <nombre> vive ?'),nl,
    tab(2),write('10. quien es hermano de <nombre>?'),nl,
    tab(2),write('11. quien es hermana de <nombre>?'),nl,
    tab(2),write('12. quienes son los ancestros de <nombre>?'),nl,
    tab(2),write('13. quien es la esposa de <nombre>?'),nl,
    tab(2),write('14. quien es tio de <nombre>?'),nl,
    tab(2),write('Que deportes le gustan a <nombre>?'),nl,
    tab(2),write('Que comidas le gustan a <nombre>?'),nl,
    tab(2),write(' Que mascota tiene <nombre>?'),nl,
    tab(2),write(' Quiénes viven en qué ciudad?'),nl,
    tab(2),write(' Quiénes viven en qué país?'),nl,
    %tab(2),write(' ¿Quiénes que viven en X país, les gusta Y deporte?'),nl,
    %tab(2),write(' ¿Quiénes que viven en X país, les gusta Y comida?'),nl,
    %tab(2),write(' ¿Quiénes que viven en X país, tienen Y mascota?'),nl,
    %tab(2),write(' ¿A qué abuelo de X, le gusta Y comida?'),nl,
    %tab(2),write(' ¿Qué abuelo de X, vive en qué ciudad?'),nl,
    %tab(2),write(' ¿Quiénes que tienen X mascotas, les gusta Y comida?'),nl,
    %tab(2),write(' ¿Qué abuelo de X, no vive?'),nl,
    write('Para salir <salir>'),nl,
    loop.

salir:-
  write('Bye!').

loop:-
    repeat,
    obtener_comando(X),
    accion(X),
    (X == salir).


accion(is_grandFather(X)):- is_grandFather(X),!.
accion(is_grandMother(X)):- is_grandMother(X),!.
accion(is_Father(X)):- is_Father(X),!.
accion(is_Mother(X)):- is_Mother(X),!.

accion(salir):- salir,!.

obtener_comando(C):-
    leer_lista(L),        
    comando(X,L,[]),
    C =.. X,!.

obtener_comando(_):-
    responder(['Reformula la pregunta.']),fail.

responder([]):-
    nl.
responder([H|T]):-
  write(H),
  responder(T).

write_list([]).
write_list([A|B]):-
    tab(2),write(A),nl,
    write_list(B).


    /* tab(2),write('6. quien es el abuelo de <nombre>?'),nl,
    tab(2),write('7. quien es la madre de <nombre>?'),nl,
    tab(2),write('9. <nombre> vive ?'),nl,
    tab(2),write('10. quien es hermano de <nombre>?'),nl,
    tab(2),write('11. quien es hermana de <nombre>?'),nl,
    tab(2),write('12. quienes son los ancestros de <nombre>?'),nl,
    tab(2),write('13. quien es la esposa de <nombre>?'),nl,
    tab(2),write('14. quien es tio de <nombre>?'),nl,
    tab(2),write('Que deportes le gustan a <nombre>?'),nl,
    tab(2),write('Que comidas le gustan a <nombre>?'),nl,
    tab(2),write(' Que mascota tiene <nombre>?'),nl,
    tab(2),write(' Quiénes viven en qué ciudad?'),nl,
    tab(2),write(' Quiénes viven en qué país?'),nl, */

comando([Relacion, Persona])--> pregunta, es, art, relacion(Relacion), de, nombre(Persona).
comando([Relacion, Persona])--> pregunta, son, art, relacion(Relacion), de, art, nombre(Persona).
comando([Relacion, Persona])--> pregunta, es, art, relacion(Relacion), de, art, nombre(Persona).
comando([Salir]) --> relacion(salir).

%comando([Relacion,Persona])--> es,nombre(Persona),padre, de, nombre(Persona).
%comando([Relacion,Persona]) --> pregunta, son, art, relacion(Relacion), de, nombre(Persona).
%comando([Relacion,Persona]) --> pregunta, son, art, relacion(Relacion), de, nombre(Persona).
%comando([Relacion,Persona]) --> pregunta, es, art, relacion(Relacion), de, nombre(Persona).
%comando([Relacion,Persona]) --> pregunta, es, relacion(Relacion), de, nombre(Persona).
%comando([Relacion,Persona]) --> pregunta, son, relacion(Relacion),art, de, nombre(Persona).
%comando([Tipo, Persona, Gusto]) --> a, que, pariente, de, nombre(Persona), art, gusta, gustos(Tipo, Gusto).
%comando([Gusto]) --> que, deportes, art, gustan, a, nombre(Persona).
%comando([Gusto]) --> que, comidas, art, gustan, a, nombre(Persona).
%comando([Relacion,Persona])--> que, mascota, tener, nombre(Persona).
%comando()--> pregunta, viven, en, nombre(Ciudad).
%comando()--> pregunta, viven, en, nombre(Pais).

%comando([Relacion, Persona]) --> pregunta, son, art, pariente, vivir(Relacion), que, tener, nombre(Persona).


mascota-->[mascotas].
pregunta --> [quienes].
pregunta --> [quien].
pregunta --> [cuales].
pariente --> [parientes].
pariente --> [familia].
pariente --> [familiares].
vivos --> [vivos].
vive --> [vive].
son --> [son].
art --> [los].
art --> [las].
art --> [el].
art --> [la].
de --> [de].
de --> [del].
juegan --> [juegan].
gusta --> [gusta].
a --> [a].
que --> [que].
mas --> [mas].
vivir(vivos) --> [vivos].
vivir(vivos) --> [vivo].
tener --> [tiene].
tener --> [tienen].
en-->[en].
es-->[es].

relacion(is_Father) --> [padre].
relacion(is_Mother) --> [madre].
relacion(is_grandFather) --> [abuelos].
relacion(is_grandFather) --> [abuelo].
relacion(is_grandMother) --> [abuelas].
relacion(is_grandMother) --> [abuela].
relacion(sister) --> [hermana].
relacion(brother) --> [hermano].
relacion(wife) --> [esposa].
relacion(uncle) --> [tio].
relacion(hijos)--> [hijos].
relacion(salir) --> [adios].
relacion(salir) --> [bye].
relacion(salir) --> [exit].

gustos(Tipo, Gusto) --> tipo_deporte(Tipo), gustos_deporte(Gusto).
tipo_deporte(gustar_deporte) --> [jugar].
gustos_deporte(Deporte) --> [Deporte], {deporte(Deporte)}.

gustos(Tipo, Gusto) --> tipo_comida(Tipo), gustos_comida(Gusto).
tipo_comida(gustar_comida) --> [comer].
gustos_comida(Comida) --> [Comida], {comida(Comida)}.

gustos(Tipo, Gusto) --> tipo_mascota(Tipo), gustos_mascota(Gusto).
gustos_mascota(Mascota) --> [Mascota], {mascota(Mascota)}.


nombre(Persona) --> [Persona], {persona(Persona)}.


leer_lista(L):-
  write('> '),
  read_word_list(L).

read_word_list([W|Ws]) :-
    get0(C),
    readword(C, W, C1),         % Read word starting with C, C1 is first new
    restsent(C1, Ws), !.        % character - use it to get rest of sentence

restsent(C,[]) :- 
    lastword(C), 
    !.                          % Nothing left if hit last-word marker
restsent(C,[W1|Ws]) :-
    readword(C,W1,C1),          % Else read next word and rest of sentence
    restsent(C1,Ws).

readword(C,W,C1) :-             % Some words are single characters
    single_char(C),             % i.e. punctuation
    !, 
    name(W, [C]),               % get as an atom
    get0(C1).
readword(C, W, C1) :-
    is_num(C),                  % if we have a number --
    !,
    number_word(C, W, C1, _).   % convert it to a genuine number
readword(C,W,C2) :-             % otherwise if character does not
    in_word(C, NewC),           % delineate end of word - keep
    get0(C1),                   % accumulating them until 
    restword(C1,Cs,C2),         % we have all the word     
    name(W, [NewC|Cs]).         % then make it an atom
readword(_,W,C2) :-             % otherwise
    get0(C1),       
    readword(C1,W,C2).        % start a new word

restword(C, [NewC|Cs], C2) :-
    in_word(C, NewC),
    get0(C1),
    restword(C1, Cs, C2).
restword(C, [], C).

single_char(0',).
single_char(0';).
single_char(0':).
single_char(0'?).
single_char(0'!).
single_char(0'.).

in_word(C, C) :- C >= 0'a, C =< 0'z.
in_word(C, C) :- C >= 0'A, C =< 0'Z, L is C + 32.
in_word(0'',0'').
in_word(0'-,0'-).
in_word(0'_,0'_).

/* number_word(C, W, C1, Pow10) :- 
    is_num(C),
    !,
    get0(C2),
    number_word(C2, W1, C1, P10),
    Pow10 is P10 * 10,
    W is integer(((C - 0'0) * Pow10) + W1).
number_word(C, 0, C, 0.1).
 */

is_num(C) :-
    C =< 0'9,
    C >= 0'0.

lastword(10). 
lastword(0'.).
lastword(0'?).




/*Comida*/
/*food(english_food).*/
food([fish_and_chips),roast_beef, full_english_breakfast, shepherds_pie, yorkshire_pudding, bangers_and_mash, black_pudding,ploughmans_lunch, chicken_tikka_masala, waffles]).

/*sports*/
/*sports(sports_list).*/
sports([football, cricket,tennis, rugby, golf, basketball, swimming, horse_riding, boxing, volleyball, ballet]).

/*pets*/
/*pets(pets_list).*/
pets([dog,cat,rabbit,horse,fish]).

/*ciudades*/
landmarks([buckingham_palace,castillo_de_praga, castillo_sforezco, palacio_de_luxemburgo ,castillo_van_beersel, castillo_san_angello,  monaco_gran_prix, zamel_wawel, piazza_de_ferrari]).
cities([londres, praga, milan, paris, bruselas, roma, montecarlo, cracovia,genovia]).
country([inglaterra, republica_checa, italia, francia, belgica, monaco, polonia]).

/*Mujeres*/
female(lady_louise).
female(princesa_beatriz).
female(princesa_eugenia).
female(savannah_phillips).
female(isla_phillips).
female(mia_tindall).
female(lena_tindall).
female(zara_phillips).
female(princesa_lilibet).
female(princesa_carlota).
female(isabel_ii).
female(princesa_ana).
female(condesa_sofia).
female(duquesa_sarah).
female(camila).
female(sienna_elizabeth).
female(duquesa_meghan).
female(princesa_diana).

/*Hombres*/
male(rey_carlos_iii).
male(felipe).
male(principe_andres).
male(principe_eduardo).
male(principe_jorge).
male(principe_luis).
male(archie).
male(peter_phillips).
male(lucas_tindall).
male(vizconde_james).
male(august_philip).
male(principe_william).
male(harry).


persona(Persona):-
    female(Persona);
    male(Persona).

/*Padres*/
parent(isabel_ii,rey_carlos_iii).
parent(felipe,rey_carlos_iii).
parent(isabel_ii,princesa_ana).
parent(felipe,princesa_ana).
parent(isabel_ii,principe_andres).
parent(felipe,principe_andres).
parent(isabel_ii,principe_eduardo).
parent(felipe,principe_eduardo).
parent(princesa_diana,principe_william).
parent(rey_carlos_iii,principe_william).
parent(princesa_diana,harry).
parent(rey_carlos_iii,harry).
parent(principe_william,principe_jorge).
parent(duquesa_catalina,principe_jorge).
parent(principe_william,princesa_carlota).
parent(duquesa_catalina,princesa_carlota).
parent(principe_william,principe_luis).
parent(duquesa_catalina,principe_luis).
parent(harry,archie).
parent(duquesa_meghan,archie).
parent(harry,princesa_lilibet).
parent(duquesa_meghan,princesa_lilibet).
parent(princesa_ana,zara_phillips).
parent(mark_phillips,zara_phillips).
parent(princesa_ana,peter_phillips).
parent(mark_phillips,peter_phillips).
parent(zara_phillips,mia_tindall).
parent(mike_tindall,mia_tindall).
parent(zara_phillips,lena_tindall).
parent(mike_tindall,lena_tindall).
parent(zara_phillips,lucas_tindall).
parent(mike_tindall,lucas_tindall).
parent(peter_phillips,savannah_phillips).
parent(autumn_kelly, savannah_phillips).
parent(peter_phillips,isla_phillips).
parent(autumn_kelly, isla_phillips).
parent(principe_andres, princesa_beatriz).
parent(duquesa_sarah, princesa_beatriz).
parent(principe_andres, princesa_eugenia).
parent(duquesa_sarah, princesa_eugenia).
parent(princesa_beatriz,sienna_elizabeth).
parent(edoardo_mapelli,sienna_elizabeth).
parent(princesa_eugenia, august_philip).
parent(jack_brooksbank, august_philip).
parent(principe_eduardo,lady_louise).
parent(condesa_sofia,lady_louise).
parent(principe_eduardo,vizconde_james).
parent(condesa_sofia,vizconde_james).


hijos(Padre):-
    (hijos_msg(Padre);
    tab(2),
    responder([Padre,' no tiene hijos registrados.']),
    fail
    ).

hijos_msg(Padre):-
    setof(Persona, father(Padre, Persona), Lista),
    responder(['Los hijos de ', Padre, ' son:']),
    write_list(Lista).

    
/**************************************Funciones de Relación***********************************/
/*quien es la mama o papa de Y*/
/*quien es el hijo hija de X*/
mother(X,Y):- parent(X,Y), female(X).  
father(X,Y):- parent(X,Y), male(X).
/*quienes son los nietos de X?*/
/*quienes son los abuelo de Z?*/
grandparent(X,Z):-  parent(X,Y),  parent(Y,Z).
/*quienes son los nietos de X?*/
/*quienes son los abuelo/abuela de Z?*/
grandmother(X,Z):-mother(X,Y),parent(Y,Z).
grandfather(X,Z):-father(X,Y),parent(Y,Z).
/*X tiene hijos?*/
haschild(X):- parent(X,_).
/*quien es la hermana hermano de Y*/
/*X es hermano de quien*/
sister(X,Y):- parent(Z,X),parent(Z,Y),female(X),X\==Y.
brother(X,Y):-parent(Z,X),parent(Z,Y),male(X),X\==Y.
/*quien es el esposo de X*/
/*quien es la esposa de Y*/
wife(X,Y):-parent(X,Z),parent(Y,Z),female(X),male(Y).
/*quien es el tio de Z*/
/*quien es el sobrino de X*/
uncle(X,Z):-brother(X,Y),parent(Y,Z).
/*quienes son los precesesores de Z*/
/*quienes son los descendientes de X*/
predecessor(X, Z) :- parent(X, Z).
predecessor(X, Z) :- parent(X, Y),predecessor(Y, Z).



/*Reportes*/
father_report:-
  write('Known fathers are:'),nl, 
  father(X,Y),
  write(X),write(' father of '),write(Y),nl,
  fail.

mother_report:-
  write('Known mothers are:'),nl,
  mother(X,Y),
  write(X),write(' mother of '),write(Y),nl,
  fail.

grandparent_report:-
  write('Known grandparents are:'),nl,
  grandparent(X,Y),
  write(X),write(' grandparent of '),write(Y),nl,
  fail.

grandchildren_report:-
  write('Known grandchildren are:'),nl,
  grandchild(X,Y),
  write(X),write(' grandchildren of '),write(Y),nl,
  fail.

/* ******************************Dead or Alive*********************************** */
dead(isabel_ii).
dead(felipe).
dead(princesa_diana).

/* ***************************************Sports******************************************** */
/*Qué deportes le gustan a X?*/
/*A quienes le gusta X deporte?*/

/* Gustos de deportes */
likes_sport(lady_louise, swimming).

likes_sport(princesa_beatriz, tennis).
likes_sport(princesa_beatriz, basketball).
likes_sport(princesa_beatriz, swimming).

likes_sport(princesa_eugenia, football).
likes_sport(princesa_eugenia, tennis).

likes_sport(savannah_phillips, swimming).
likes_sport(savannah_phillips, horse_riding).

likes_sport(isla_phillips, ballet).
likes_sport(isla_phillips, swimming).
likes_sport(isla_phillips, tennis).

likes_sport(mia_tindall, rugby).
likes_sport(mia_tindall, horse_riding).
likes_sport(mia_tindall, swimming).

likes_sport(lena_tindall, football).
likes_sport(lena_tindall, tennis).
likes_sport(lena_tindall, swimming).

likes_sport(zara_phillips, horse_riding).
likes_sport(zara_phillips, rugby).

likes_sport(princesa_lilibet, tennis).
likes_sport(princesa_lilibet, swimming).
likes_sport(princesa_lilibet, horse_riding).

likes_sport(princesa_carlota, ballet).
likes_sport(princesa_carlota, swimming).

likes_sport(isabel_ii, cricket).
likes_sport(isabel_ii, horse_riding).
likes_sport(isabel_ii, swimming).

likes_sport(princesa_ana, swimming).
likes_sport(princesa_ana, horse_riding).

likes_sport(condesa_sofia, football).
likes_sport(condesa_sofia, tennis).
likes_sport(condesa_sofia, swimming).

likes_sport(duquesa_sarah, polo).
likes_sport(duquesa_sarah, horse_riding).

likes_sport(rey_carlos_iii, polo).
likes_sport(rey_carlos_iii, rugby).

likes_sport(felipe, basketball).
likes_sport(felipe, swimming).

likes_sport(principe_andres, rugby).
likes_sport(principe_andres, golf).

likes_sport(principe_eduardo, tennis).
likes_sport(principe_eduardo, rugby).

likes_sport(principe_jorge, football).
likes_sport(principe_jorge, basketball).

likes_sport(principe_luis, swimming).
likes_sport(principe_luis, cricket).

likes_sport(archie, rugby).
likes_sport(archie, football).

likes_sport(peter_phillips, rugby).
likes_sport(peter_phillips, cricket).

likes_sport(lucas_tindall, swimming).
likes_sport(lucas_tindall, tennis).

likes_sport(vizconde_james, cricket).
likes_sport(vizconde_james, golf).

likes_sport(august_philip, football).
likes_sport(august_philip, tennis).

likes_sport(principe_william, football).
likes_sport(principe_william, basketball).

likes_sport(harry, rugby).
likes_sport(harry, polo).


/************************************* Gustos de comida ***************************************/
/*Qué comidas le gustan a X?*/
/*A quienes le gusta X comida?*/
likes_food(lady_louise, fish_and_chips).
likes_food(lady_louise, roast_beef).
likes_food(lady_louise, full_english_breakfast).

likes_food(princesa_beatriz, roast_beef).
likes_food(princesa_beatriz, yorkshire_pudding).
likes_food(princesa_beatriz, bangers_and_mash).

likes_food(princesa_eugenia, fish_and_chips).
likes_food(princesa_eugenia, ploughmans_lunch).
likes_food(princesa_eugenia, chicken_tikka_masala).

likes_food(savannah_phillips, roast_beef).
likes_food(savannah_phillips, shepherds_pie).
likes_food(savannah_phillips, chicken_tikka_masala).

likes_food(isla_phillips, fish_and_chips).
likes_food(isla_phillips, waffles).
likes_food(isla_phillips, black_pudding).

likes_food(mia_tindall, roast_beef).
likes_food(mia_tindall, full_english_breakfast).
likes_food(mia_tindall, ploughmans_lunch).

likes_food(lena_tindall, fish_and_chips).
likes_food(lena_tindall, yorkshire_pudding).
likes_food(lena_tindall, bangers_and_mash).

likes_food(zara_phillips, roast_beef).
likes_food(zara_phillips, shepherds_pie).
likes_food(zara_phillips, chicken_tikka_masala).

likes_food(princesa_lilibet, fish_and_chips).
likes_food(princesa_lilibet, ploughmans_lunch).
likes_food(princesa_lilibet, yorkshire_pudding).

likes_food(princesa_carlota, roast_beef).
likes_food(princesa_carlota, waffles).
likes_food(princesa_carlota, black_pudding).

likes_food(isabel_ii, fish_and_chips).
likes_food(isabel_ii, full_english_breakfast).
likes_food(isabel_ii, ploughmans_lunch).

likes_food(princesa_ana, roast_beef).
likes_food(princesa_ana, shepherds_pie).
likes_food(princesa_ana, chicken_tikka_masala).

likes_food(condesa_sofia, fish_and_chips).
likes_food(condesa_sofia, yorkshire_pudding).
likes_food(condesa_sofia, bangers_and_mash).

likes_food(duquesa_sarah, roast_beef).
likes_food(duquesa_sarah, full_english_breakfast).
likes_food(duquesa_sarah, ploughmans_lunch).

likes_food(rey_carlos_iii, fish_and_chips).
likes_food(rey_carlos_iii, shepherds_pie).
likes_food(rey_carlos_iii, chicken_tikka_masala).

likes_food(felipe, roast_beef).
likes_food(felipe, yorkshire_pudding).
likes_food(felipe, bangers_and_mash).

likes_food(principe_andres, fish_and_chips).
likes_food(principe_andres, ploughmans_lunch).
likes_food(principe_andres, yorkshire_pudding).

likes_food(principe_eduardo, roast_beef).
likes_food(principe_eduardo, shepherds_pie).
likes_food(principe_eduardo, chicken_tikka_masala).

likes_food(principe_jorge, fish_and_chips).
likes_food(principe_jorge, yorkshire_pudding).
likes_food(principe_jorge, bangers_and_mash).

likes_food(principe_luis, roast_beef).
likes_food(principe_luis, full_english_breakfast).
likes_food(principe_luis, ploughmans_lunch).

likes_food(archie, fish_and_chips).
likes_food(archie, waffles).
likes_food(archie, black_pudding).

likes_food(peter_phillips, roast_beef).
likes_food(peter_phillips, shepherds_pie).
likes_food(peter_phillips, chicken_tikka_masala).

likes_food(lucas_tindall, fish_and_chips).
likes_food(lucas_tindall, ploughmans_lunch).
likes_food(lucas_tindall, yorkshire_pudding).

likes_food(vizconde_james, roast_beef).
likes_food(vizconde_james, full_english_breakfast).
likes_food(vizconde_james, ploughmans_lunch).

likes_food(august_philip, fish_and_chips).
likes_food(august_philip, shepherds_pie).
likes_food(august_philip, chicken_tikka_masala).

likes_food(principe_william, roast_beef).
likes_food(principe_william, yorkshire_pudding).
likes_food(principe_william, bangers_and_mash).

likes_food(harry, fish_and_chips).
likes_food(harry, ploughmans_lunch).
likes_food(harry, black_pudding).




/* ***********************************Mascotas *********************************/
/*Qué mascota tiene x?*/
has_pet(lady_louise, dog).

has_pet(princesa_beatriz, dog).
has_pet(princesa_beatriz, rabbit).

has_pet(princesa_eugenia, dog).
has_pet(princesa_eugenia, fish).

has_pet(savannah_phillips, dog).

has_pet(isla_phillips, dog).
has_pet(isla_phillips, cat).

has_pet(mia_tindall, dog).
has_pet(mia_tindall, rabbit).

has_pet(lena_tindall, dog).

has_pet(zara_phillips, dog).
has_pet(zara_phillips, horse).

has_pet(princesa_lilibet, dog).
has_pet(princesa_lilibet, rabbit).

has_pet(princesa_carlota, dog).
has_pet(princesa_carlota, cat).

has_pet(isabel_ii, dog).
has_pet(isabel_ii, horse).

has_pet(princesa_ana, dog).
has_pet(princesa_ana, horse).

has_pet(condesa_sofia, dog).

has_pet(duquesa_sarah, dog).
has_pet(duquesa_sarah, cat).

has_pet(rey_carlos_iii, dog).
has_pet(rey_carlos_iii, horse).

has_pet(felipe, dog).

has_pet(principe_andres, dog).
has_pet(principe_andres, rabbit).

has_pet(principe_eduardo, dog).

has_pet(principe_jorge, dog).
has_pet(principe_jorge, cat).

has_pet(principe_luis, dog).

has_pet(archie, dog).
has_pet(archie, rabbit).

has_pet(peter_phillips, dog).

has_pet(lucas_tindall, cat).

has_pet(vizconde_james, cat).

has_pet(august_philip, dog).

has_pet(principe_william, dog).
has_pet(principe_william, cat).

has_pet(harry, dog).
has_pet(harry, rabbit).


/**************VIVIENDA********************/

/*landmarks([buckingham_palace,castillo_de_praga, castillo_sforezco, palacio_de_luxemburgo ,kasteel_van_beersel, castillo_san_angello,  monaco_gran_prix, zamel_wawel, piazza_de_ferrari]).
cities([londres, praga, milan, paris, bruselas, roma, montecarlo, cracovia,genovia]).
country([inglaterra, republica_checa, italia, francia, belgica, monaco, polonia]).*/




live_at(rey_carlos_iii,buckingham_palace).
live_at(camila,buckingham_palace).

live_at(principe_eduardo,castillo_de_praga).
live_at(condesa_sofia,castillo_de_praga).
live_at(lady_louise,castillo_de_praga).
live_at(vizconde_james,castillo_de_praga).

live_at(princesa_eugenia,castillo_sforezco).
live_at(august_philip,castillo_sforezco).
live_at(jack_brooksbank,castillo_sforezco).

live_at(princesa_beatriz,palacio_de_luxemburgo).
live_at(sienna_elizabeth,palacio_de_luxemburgo).
live_at(edoardo_mapelli,palacio_de_luxemburgo).

live_at(principe_andres,monaco_gran_prix).

live_at(duquesa_sarah,palacio_de_luxemburgo).

live_at(princesa_ana,piazza_de_ferrari).

live_at(mark_phillips,castillo_san_angello).

live_at(zara_phillips,kasteel_van_beersel).
live_at(mike_tindall,kasteel_van_beersel).
live_at(mia_tindall,kasteel_van_beersel).
live_at(lena_tindall,kasteel_van_beersel).
live_at(lucas_tindall,kasteel_van_beersel).

live_at(peter_phillips,castillo_san_angello).
live_at(autumn_kelly,castillo_san_angello).
live_at(isla_phillips,castillo_san_angello).
live_at(savannah_phillips,castillo_san_angello).

live_at(harry,zamel_wawel).
live_at(archie,zamel_wawel).
live_at(duquesa_meghan,zamel_wawel).
live_at(princesa_lilibet,zamel_wawel).

live_at(principe_william,buckingham_palace).
live_at(duquesa_catalina,buckingham_palace).
live_at(principe_luis,buckingham_palace).
live_at(princesa_carlota,buckingham_palace).
live_at(principe_jorge,buckingham_palace).

/* Clasificar ciudades en ciudades, provincias y países */
landmark_from(buckingham_palace, londres).
landmark_from(castillo_de_praga,praga).
landmark_from(castillo_sforezco,milan).
landmark_from(palacio_de_luxemburgo,paris).
landmark_from(kasteel_van_beersel,bruselas).
landmark_from(castillo_san_angello,roma).
landmark_from(monaco_gran_prix,montecarlo).
landmark_from(zamel_wawel,cracovia).
landmark_from(piazza_de_ferrarigenovia).

city_from(londres,inglaterra).
city_from(praga, republica_checa).
city_from(milan, italia).
city_from(paris, francia).
city_from(bruselas, belgica).
city_from(roma, italia).
city_from(montecarlo,monaco).
city_from(cracovia, polonia).
city_from(genovia, italia).


/* Quiénes viven en qué ciudad? */
residents_in_city(City, People) :-
    landmark_from(Landmark, City),
    live_at(People, Landmark).

/* Quiénes viven en qué país? */
residents_in_country(Country, People) :-
    city_from(City, Country),
    residents_in_city(City, People).

/*¿Quiénes que viven en X país, les gusta Y deporte?*/
who_sport(Country,Lsport,People) :-
    residents_in_country(Country, People),
    likes_sport(People,Lsport).

/*¿Quiénes que viven en X país, les gusta Y comida?*/
who_food(Country,Lfood,People) :-
    residents_in_country(Country, People),
    likes_food(People,Lfood).

/*¿Quiénes que viven en X país, tienen Y mascota?*/
who_pet(Country,Hpet,People) :-
    residents_in_country(Country, People),
    has_pet(People,Hpet).

/*¿A qué abuelo de X, le gusta Y comida?*/
who_gparent(Person,Lfood,Grandparent) :-
    grandparent(Grandparent,Person),
    likes_food(Person,Lfood).  
  
/*¿Qué abuelo de X, vive en qué ciudad?*/
who_gparent_liveat(Person,Lcity,Grandparent) :-
    grandparent(Grandparent,Person),
    residents_in_city(City,Person).

/*¿Quiénes que tienen X mascotas, les gusta Y comida?*/
who_petowner_likesfood(Country,Hpet,People) :-
    likes_food(People,food),
    has_pet(People,Hpet).

/*¿Qué abuelo de X, no vive?*/
who_gparent_nalive(Person,Grandparent) :-
    grandparent(Grandparent,Person),
    dead(Person).


is_grandFather(Person):-
    grandfather(X, Person),
    write(X),nl.

is_grandMother(Person):-
    grandmother(X, Person),
    write(X),nl.

is_Father(Person):-
    father(X, Person),
    write(X),nl.

is_Mother(Person):-
    mother(X, Person),
    write(X),nl.