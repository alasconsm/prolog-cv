/*Comida*/
/*food(english_food).*/
food(['fish_and_chips', 'roast_beef', 'full_english_breakfast', 'shepherds_pie', 'yorkshire_pudding', 'bangers_and_mash', 'black_pudding','ploughmans_lunch', 'chicken_tikka_masala', 'waffles']).

/*sports*/
/*sports(sports_list).*/
sports(['football', 'cricket', 'tennis', 'rugby', 'golf', 'basketball', 'swimming', 'horse_riding', 'boxing', 'volleyball', 'ballet']).

/*pets*/
/*pets(pets_list).*/
pets(['dog', 'cat', 'rabbit', 'horse','fish']).

/*ciudades*/
landmarks(['Buckingham Palace','Castillo de Praga', 'Sforezco Castle', 'Luxembourg Palace' ,'Kasteel van Beersel', 'Castel Sant-Angelo',  'Monaco Grand Prix', 'Zamel Wawel', 'Piazza de Ferrari' ]).
cities(['Londres', 'Praga', 'Milan', 'Paris', 'Bruselas', 'Roma', 'Montecarlo', 'Cracovia','Genovia']).
country(['Inglaterra', 'Republica_Checa', 'Italia', 'Francia', 'Belgica', 'Monaco', 'Polonia']).

/* Clasificar ciudades en ciudades, provincias y países */
landmark_from('Buckingham Palace', 'Londres').
landmark_from('Castillo de Praga','Praga').
landmark_from('Sforezco Castle','Milan').
landmark_from('Luxembourg Palace','Paris').
landmark_from('Kasteel van Beersel','Bruselas').
landmark_from('Castel Sant-Angelo','Roma').
landmark_from('Monaco Grand Prix','Montecarlo').
landmark_from('Zamel Wawel','Cracovia').
landmark_from('Piazza de Ferrari','Genovia').

city_from('Londres','Inglaterra').
city_from('Praga', 'Republica_Checa').
city_from('Milan', 'Italia').
city_from('Paris', 'Francia').
city_from('Bruselas', 'Belgica').
city_from('Roma', 'Italia').
city_from('Montecarlo','Monaco').
city_from('Cracovia', 'Polonia').
city_from('Genovia', 'Italia').


/*Mujeres*/
female('Lady Louise').
female('Princesa Beatriz').
female('Princesa Eugenia').
female('Savannah Phillips').
female('Isla Phillips').
female('Mia Tindall').
female('Lena Tindall').
female('Zara Phillips').
female('Princesa Lilibet').
female('Princesa Carlota').
female('Isabel II').
female('Princesa Ana').
female('Condesa Sofia').
female('Duquesa Sarah').
female('Camila').

/*Hombres*/
male('Rey Carlos III').
male('Felipe').
male('Principe Andres').
male('Principe Eduardo').
male('Principe Jorge').
male('Principe Luis').
male('Principe Archie').
male('Peter Phillips').
male('Lucas Tindall').
male('Vizconde James').
male('August Philip').
male('Principe William').
male('Duque Harry').

/*Padres*/
parent('Isabel II','Rey Carlos III').
parent('Felipe','Rey Carlos III').
parent('Isabel II','Princesa Ana').
parent('Felipe','Princesa Ana').
parent('Isabel II','Principe Andres').
parent('Felipe','Principe Andres').
parent('Isabel II','Principe Eduardo').
parent('Felipe','Principe Eduardo').
parent('Princesa Diana','Principe William').
parent('Rey Carlos III','Principe William').
parent('Princesa Diana','Duque Harry').
parent('Rey Carlos III','Duque Harry').
parent('Principe William','Principe Jorge').
parent('Duquesa Catalina','Principe Jorge').
parent('Principe William','Princesa Carlota').
parent('Duquesa Catalina','Princesa Carlota').
parent('Principe William','Principe Luis').
parent('Duquesa Catalina','Principe Luis').
parent('Duque Harry','Principe Archie').
parent('Duquesa Meghan','Principe Archie').
parent('Duque Harry','Princesa Lilibet').
parent('Duquesa Meghan','Princesa Lilibet').
parent('Princesa Ana','Zara Phillips').
parent('Mark Phillips','Zara Phillips').
parent('Princesa Ana','Peter Phillips').
parent('Mark Phillips','Peter Phillips').
parent('Zara Phillips','Mia Tindall').
parent('Mike Tindall','Mia Tindall').
parent('Zara Phillips','Lena Tindall').
parent('Mike Tindall','Lena Tindall').
parent('Zara Phillips','Lucas Tindall').
parent('Mike Tindall','Lucas Tindall').
parent('Peter Phillips','Savannah Phillips').
parent('Autumn Kelly', 'Savannah Phillips').
parent('Peter Phillips','Isla Phillips').
parent('Autumn Kelly', 'Isla Phillips').
parent('Principe Andres', 'Princesa Beatriz').
parent('Duquesa Sarah', 'Princesa Beatriz').
parent('Principe Andres', 'Princesa Eugenia').
parent('Duquesa Sarah', 'Princesa Eugenia').
parent('Princesa Beatriz','Sienna Elizabeth').
parent('Edoardo Mapelli','Sienna Elizabeth').
parent('Princesa Eugenia', 'August Philip').
parent('Jack Brooksbank', 'August Philip').
parent('Principe Eduardo','Lady Louise').
parent('Condesa Sofia','Lady Louise').
parent('Principe Eduardo','Vizconde James').
parent('Condesa Sofia','Vizconde James').


/**************************************Funciones de Relación***********************************/
/*Quien es la mama o papa de Y*/
/*Quien es el hijo hija de X*/
mother(X,Y):- parent(X,Y), female(X).  
father(X,Y):- parent(X,Y), male(X).
/*Quienes son los nietos de X?*/
/*Quienes son los abuelo de Z?*/
grandparent(X,Z):-  parent(X,Y),  parent(Y,Z).
/*Quienes son los nietos de X?*/
/*Quienes son los abuelo/abuela de Z?*/
grandmother(X,Z):-mother(X,Y),parent(Y,Z).
grandfather(X,Z):-father(X,Y),parent(Y,Z).
/*X tiene hijos?*/
haschild(X):- parent(X,_).
/*Quien es la hermana hermano de Y*/
/*X es hermano de quien*/
sister(X,Y):- parent(Z,X),parent(Z,Y),female(X),X\==Y.
brother(X,Y):-parent(Z,X),parent(Z,Y),male(X),X\==Y.
/*Quien es el esposo de X*/
/*quien es la esposa de Y*/
wife(X,Y):-parent(X,Z),parent(Y,Z),female(X),male(Y).
/*Quien es el tio de Z*/
/*Quien es el sobrino de X*/
uncle(X,Z):-brother(X,Y),parent(Y,Z).
/*Quienes son los precesesores de Z*/
/*Quienes son los descendientes de X*/
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
dead('Isabel II').
dead('Felipe').
dead('Princesa Diana').

/* ***************************************Sports******************************************** */
/*Qué deportes le gustan a X?*/
/*A quienes le gusta X deporte?*/

/* Gustos de deportes */
likes_sport('Lady Louise', swimming).

likes_sport('Princesa Beatriz', tennis).
likes_sport('Princesa Beatriz', basketball).
likes_sport('Princesa Beatriz', swimming).

likes_sport('Princesa Eugenia', football).
likes_sport('Princesa Eugenia', tennis).

likes_sport('Savannah Phillips', swimming).
likes_sport('Savannah Phillips', horse_riding).

likes_sport('Isla Phillips', ballet).
likes_sport('Isla Phillips', swimming).
likes_sport('Isla Phillips', tennis).

likes_sport('Mia Tindall', rugby).
likes_sport('Mia Tindall', horse_riding).
likes_sport('Mia Tindall', swimming).

likes_sport('Lena Tindall', football).
likes_sport('Lena Tindall', tennis).
likes_sport('Lena Tindall', swimming).

likes_sport('Zara Phillips', horse_riding).
likes_sport('Zara Phillips', rugby).

likes_sport('Princesa Lilibet', tennis).
likes_sport('Princesa Lilibet', swimming).
likes_sport('Princesa Lilibet', horse_riding).

likes_sport('Princesa Carlota', ballet).
likes_sport('Princesa Carlota', swimming).

likes_sport('Isabel II', cricket).
likes_sport('Isabel II', horse_riding).
likes_sport('Isabel II', swimming).

likes_sport('Princesa Ana', swimming).
likes_sport('Princesa Ana', horse_riding).

likes_sport('Condesa Sofia', football).
likes_sport('Condesa Sofia', tennis).
likes_sport('Condesa Sofia', swimming).

likes_sport('Duquesa Sarah', polo).
likes_sport('Duquesa Sarah', horse_riding).

likes_sport('Rey Carlos III', polo).
likes_sport('Rey Carlos III', rugby).

likes_sport('Felipe', basketball).
likes_sport('Felipe', swimming).

likes_sport('Principe Andres', rugby).
likes_sport('Principe Andres', golf).

likes_sport('Principe Eduardo', tennis).
likes_sport('Principe Eduardo', rugby).

likes_sport('Principe Jorge', football).
likes_sport('Principe Jorge', basketball).

likes_sport('Principe Luis', swimming).
likes_sport('Principe Luis', cricket).

likes_sport('Principe Archie', rugby).
likes_sport('Principe Archie', football).

likes_sport('Peter Phillips', rugby).
likes_sport('Peter Phillips', cricket).

likes_sport('Lucas Tindall', swimming).
likes_sport('Lucas Tindall', tennis).

likes_sport('Vizconde James', cricket).
likes_sport('Vizconde James', golf).

likes_sport('August Philip', football).
likes_sport('August Philip', tennis).

likes_sport('Principe William', football).
likes_sport('Principe William', basketball).

likes_sport('Duque Harry', rugby).
likes_sport('Duque Harry', polo).


/************************************* Gustos de comida ***************************************/
/*Qué comidas le gustan a X?*/
/*A quienes le gusta X comida?*/
likes_food('Lady Louise', fish_and_chips).
likes_food('Lady Louise', roast_beef).
likes_food('Lady Louise', full_english_breakfast).

likes_food('Princesa Beatriz', roast_beef).
likes_food('Princesa Beatriz', yorkshire_pudding).
likes_food('Princesa Beatriz', bangers_and_mash).

likes_food('Princesa Eugenia', fish_and_chips).
likes_food('Princesa Eugenia', ploughmans_lunch).
likes_food('Princesa Eugenia', chicken_tikka_masala).

likes_food('Savannah Phillips', roast_beef).
likes_food('Savannah Phillips', shepherds_pie).
likes_food('Savannah Phillips', chicken_tikka_masala).

likes_food('Isla Phillips', fish_and_chips).
likes_food('Isla Phillips', waffles).
likes_food('Isla Phillips', black_pudding).

likes_food('Mia Tindall', roast_beef).
likes_food('Mia Tindall', full_english_breakfast).
likes_food('Mia Tindall', ploughmans_lunch).

likes_food('Lena Tindall', fish_and_chips).
likes_food('Lena Tindall', yorkshire_pudding).
likes_food('Lena Tindall', bangers_and_mash).

likes_food('Zara Phillips', roast_beef).
likes_food('Zara Phillips', shepherds_pie).
likes_food('Zara Phillips', chicken_tikka_masala).

likes_food('Princesa Lilibet', fish_and_chips).
likes_food('Princesa Lilibet', ploughmans_lunch).
likes_food('Princesa Lilibet', yorkshire_pudding).

likes_food('Princesa Carlota', roast_beef).
likes_food('Princesa Carlota', waffles).
likes_food('Princesa Carlota', black_pudding).

likes_food('Isabel II', fish_and_chips).
likes_food('Isabel II', full_english_breakfast).
likes_food('Isabel II', ploughmans_lunch).

likes_food('Princesa Ana', roast_beef).
likes_food('Princesa Ana', shepherds_pie).
likes_food('Princesa Ana', chicken_tikka_masala).

likes_food('Condesa Sofia', fish_and_chips).
likes_food('Condesa Sofia', yorkshire_pudding).
likes_food('Condesa Sofia', bangers_and_mash).

likes_food('Duquesa Sarah', roast_beef).
likes_food('Duquesa Sarah', full_english_breakfast).
likes_food('Duquesa Sarah', ploughmans_lunch).

likes_food('Rey Carlos III', fish_and_chips).
likes_food('Rey Carlos III', shepherds_pie).
likes_food('Rey Carlos III', chicken_tikka_masala).

likes_food('Felipe', roast_beef).
likes_food('Felipe', yorkshire_pudding).
likes_food('Felipe', bangers_and_mash).

likes_food('Principe Andres', fish_and_chips).
likes_food('Principe Andres', ploughmans_lunch).
likes_food('Principe Andres', yorkshire_pudding).

likes_food('Principe Eduardo', roast_beef).
likes_food('Principe Eduardo', shepherds_pie).
likes_food('Principe Eduardo', chicken_tikka_masala).

likes_food('Principe Jorge', fish_and_chips).
likes_food('Principe Jorge', yorkshire_pudding).
likes_food('Principe Jorge', bangers_and_mash).

likes_food('Principe Luis', roast_beef).
likes_food('Principe Luis', full_english_breakfast).
likes_food('Principe Luis', ploughmans_lunch).

likes_food('Principe Archie', fish_and_chips).
likes_food('Principe Archie', waffles).
likes_food('Principe Archie', black_pudding).

likes_food('Peter Phillips', roast_beef).
likes_food('Peter Phillips', shepherds_pie).
likes_food('Peter Phillips', chicken_tikka_masala).

likes_food('Lucas Tindall', fish_and_chips).
likes_food('Lucas Tindall', ploughmans_lunch).
likes_food('Lucas Tindall', yorkshire_pudding).

likes_food('Vizconde James', roast_beef).
likes_food('Vizconde James', full_english_breakfast).
likes_food('Vizconde James', ploughmans_lunch).

likes_food('August Philip', fish_and_chips).
likes_food('August Philip', shepherds_pie).
likes_food('August Philip', chicken_tikka_masala).

likes_food('Principe William', roast_beef).
likes_food('Principe William', yorkshire_pudding).
likes_food('Principe William', bangers_and_mash).

likes_food('Duque Harry', fish_and_chips).
likes_food('Duque Harry', ploughmans_lunch).
likes_food('Duque Harry', black_pudding).



/* ***********************************Mascotas *********************************/
/*Qué mascota tiene x?*/
has_pet('Lady Louise', dog).

has_pet('Princesa Beatriz', dog).
has_pet('Princesa Beatriz', rabbit).

has_pet('Princesa Eugenia', dog).
has_pet('Princesa Eugenia', fish).

has_pet('Savannah Phillips', dog).

has_pet('Isla Phillips', dog).
has_pet('Isla Phillips', cat).

has_pet('Mia Tindall', dog).
has_pet('Mia Tindall', rabbit).

has_pet('Lena Tindall', dog).

has_pet('Zara Phillips', dog).
has_pet('Zara Phillips', horse).

has_pet('Princesa Lilibet', dog).
has_pet('Princesa Lilibet', rabbit).

has_pet('Princesa Carlota', dog).
has_pet('Princesa Carlota', cat).

has_pet('Isabel II', dog).
has_pet('Isabel II', horse).

has_pet('Princesa Ana', dog).
has_pet('Princesa Ana', horse).

has_pet('Condesa Sofia', dog).

has_pet('Duquesa Sarah', dog).
has_pet('Duquesa Sarah', cat).

has_pet('Rey Carlos III', dog).
has_pet('Rey Carlos III', horse).

has_pet('Felipe', dog).

has_pet('Principe Andres', dog).
has_pet('Principe Andres', rabbit).

has_pet('Principe Eduardo', dog).

has_pet('Principe Jorge', dog).
has_pet('Principe Jorge', cat).

has_pet('Principe Luis', dog).

has_pet('Principe Archie', dog).
has_pet('Principe Archie', rabbit).

has_pet('Peter Phillips', dog).

has_pet('Lucas Tindall', cat).

has_pet('Vizconde James', cat).

has_pet('August Philip', dog).

has_pet('Principe William', dog).
has_pet('Principe William', cat).

has_pet('Duque Harry', dog).
has_pet('Duque Harry', rabbit).


/**************VIVIENDA********************/

# landmarks(['Buckingham Palace','Castillo de Praga', 'Sforezco Castle', 'Luxembourg Palace' ,'Kasteel van Beersel', 'Castel Sant-Angelo',  'Monaco Grand Prix', 'Zamel Wawel', 'Piazza de Ferrari' ]).
# cities(['Londres', 'Praga', 'Milan', 'Paris', 'Bruselas', 'Roma', 'Montecarlo', 'Cracovia','Genovia']).
# country(['Inglaterra', 'Republica_Checa', 'Italia', 'Francia', 'Belgica', 'Monaco', 'Polonia']).


live_at('Rey Carlos III','Buckingham Palace').
live_at('Camila','Buckingham Palace').

live_at('Principe Eduardo','').
live_at('Condesa Sofia','').
live_at('Lady Louise','').
live_at('Vizconde James','').

live_at('Princesa Eugenia','').
live_at('August Philip','').
live_at('Jack Brooksbank','').

live_at('Princesa Beatriz','').
live_at('Sienna Elizabeth','').
live_at('Edoardo Mapelli','').

live_at('Principe Andres','').

live_at('Duquesa Sarah','').

live_at('Timothy Laurence','').
live_at('Princesa Ana','').

live_at('Mark Phillips','').

live_at('Zara Phillips','').
live_at('Mike Tindall','').
live_at('Mia Tindall','').
live_at('Lena Tindall','').
live_at('Lucas Tindall','').

live_at('Peter Phillips','').
live_at('Autumn Kelly','').
live_at('Isla Phillips','').
live_at('Savannah Phillips','').

live_at('Duque Harry','').
live_at('Principe Archie','').
live_at('Duquesa Meghan','').
live_at('Princesa Lilibet','').

live_at('Principe William','').
live_at('Duquesa Catalina','').
live_at('Principe Luis','').
live_at('Princesa Carlota','').
live_at('Principe Jorge','').