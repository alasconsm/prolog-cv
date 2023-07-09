# Base de Datos Genealógica - Familia Real
### Alisson Constantine & Nelson Vera

El presente repositorio permite consultar el árbol genealógico de la familia real Británica, actualizado al 2023 con el actual Rey Carlos III.

#### Para ejecutar el programa utilice: 
swipl .\familiareal.pl.

#### Preguntas que se pueden realizar:
Reemplace X o Y por el nombre de la persona por la qué desea consultar.
##### 1.¿Quién es la mamá o papá de Y?
##### 2.¿Quién es el hijo o hija de X?
mother(X,Y).  
father(X,Y).

##### 3.¿Quiénes son los nietos de X?
##### 4.¿Quiénes son los abuelos de Z?
grandparent(X,Z).
grandmother(X,Z).
grandfather(X,Z).

##### 5.¿X tiene hijos?
haschild(X).

##### 6.¿Quién es el hermano/a de Y?
##### 7.¿De quién es hermano/a X?
sister(X,Y).
brother(X,Y).

##### 8.¿Quién es el esposo de X?
##### 9.¿Quien es la esposa de Y?
wife(X,Y).

##### 10.¿Quién es el tio de Z?
##### 11.¿Quién es el sobrino de X?
uncle(X,Z).

##### 12.¿Quienes son los precesesores de Z?
##### 13.¿Quienes son los descendientes de X?
predecessor(X, Z).


