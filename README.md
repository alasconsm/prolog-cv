# Base de Datos Genealógica - Familia Real
### Alisson Constantine & Nelson Vera

/*El presente repositorio permite consultar el árbol genealógico de la familia real Británica, actualizado al 2023 con el actual Rey Carlos III.*/

#### Para ejecutar el programa utilice: 
swipl .\familiareal.pl.
#### Para actualizar el programa desde la consola utilice: 
consult('familiareal.pl').

#### Preguntas que se pueden realizar:
/*Reemplace X o Y por el nombre de la persona por la qué desea consultar.*/

##### 1.¿Quién es la mamá o papá de Y? ó ¿Quién es el hijo o hija de X?
mother(X,Y).  
father(X,Y).

##### 2.¿Quiénes son los nietos de X? ó ¿Quiénes son los abuelos de Z?
grandparent(X,Z).
grandmother(X,Z).
grandfather(X,Z).

##### 3.¿X tiene hijos?
haschild(X).

##### 4.¿Quién es el hermano/a de Y? ó ¿De quién es hermano/a X?
sister(X,Y).
brother(X,Y).

##### 5.¿Quién es el esposo de X? ó ¿Quien es la esposa de Y?
wife(X,Y).

##### 6.¿Quién es el tio de Z? ó ¿Quién es el sobrino de X?
uncle(X,Z).

##### 7.¿Quienes son los precesesores de Z? ó ¿Quienes son los descendientes de X?
predecessor(X, Z).

##### 8.¿Está X aún con vida?
dead(X).

##### 9.¿Qué deportes les gusta a X? ó ¿A quién le gusta Y deporte? 
###### Deportes disponibles: 'football', 'cricket', 'tennis', 'rugby', 'golf', 'basketball', 'swimming', 'horse_riding', 'boxing', 'volleyball', 'ballet'
likes_sport(X, Y).

##### 10.¿Qué comidas le gustan a X? ó ¿A quiénes le gusta Y comida?*/
###### Platillos disponibles: 'fish_and_chips', 'roast_beef', 'full_english_breakfast', 'shepherds_pie', 'yorkshire_pudding', 'bangers_and_mash', 'black_pudding','ploughmans_lunch', 'chicken_tikka_masala', 'waffles'
likes_food(X, Y).

##### 11¿Qué mascota tiene X? ó ¿Quiénes tienen Y mascota?
###### Mascotas disponibles: 'dog', 'cat', 'rabbit', 'horse','fish'
has_pet(X,Y).

#### 12.¿En qué castillo vive X? ó ¿Quiénes viven en Y castillo?
##### Castillos disponibles: 'Buckingham Palace','Castillo de Praga', 'Sforezco Castle', 'Luxembourg Palace' ,'Kasteel van Beersel', 'Castel Sant-Angelo',  'Monaco Grand Prix', 'Zamel Wawel', 'Piazza de Ferrari'
live_at(X,Y).

#### 13.¿Quiénes viven en X país? ó ¿En qué país vive Y?
residents_in_country(X, Y).

#### 14.¿Quiénes viven en X ciudad? o ¿En qué ciudad vive Y?.
residents_in_city(X, Y).

#### 15.¿Quiénes que viven en X país, les gusta Y deporte?
who_sport(Country,Lsport,People).

#### 16.¿Quiénes que viven en X país, les gusta Y comida?
who_food(Country,Lfood,People)

#### 17.¿Quiénes que viven en X país, tienen Y mascota?
who_pet(Country,Hpet,People)

#### 18.¿A qué abuelo/a de X, le gusta Y comida?
who_gparent(Person,Lfood,Grandparent)
