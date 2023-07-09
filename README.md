# Base de Datos Genealógica - Familia Real
### Alisson Constantine & Nelson Vera

El presente repositorio permite consultar el árbol genealógico de la familia real Británica, actualizado al 2023 con el actual Rey Carlos III.

#### Para ejecutar el programa utilice: 
swipl .\familiareal.pl.

#### Preguntas que se pueden realizar:
Reemplace X o Y por el nombre de la persona por la qué desea consultar.

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


