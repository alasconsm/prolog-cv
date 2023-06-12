/* Padres*/

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


mother(X,Y):- 
  parent(X,Y),
  female(X).

father(X,Y):- 
  parent(X,Y),
  male(X).

grandparent(X,Z):-
  parent(X,Y),
  parent(Y,Z).

grandchild(X,Z):-
  grandparent(Z,X).

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