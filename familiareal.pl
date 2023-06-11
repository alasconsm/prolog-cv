/* Padres*/

parent('Isabel II','Rey Carlos III').
parent('Felipe','Rey Carlos III').

parent('Isabel II','Princesa Ana').
parent('Felipe','Princesa Ana').

parent('Isabel II','Príncipe Andrés').
parent('Felipe','Príncipe Andrés').

parent('Isabel II','Príncipe Eduardo').
parent('Felipe','Príncipe Eduardo').


female('Isabel II').
female('Princesa Ana').
male('Rey Carlos III').
male('Felipe').
male('Príncipe Andrés').
male('Príncipe Eduardo').


parent('Princesa Diana','Príncipe William').
parent('Rey Carlos III','Príncipe William').

parent('Princesa Diana','Duque Harry').
parent('Rey Carlos III','Duque Harry').

male('Príncipe William').
male('Duque Harry').


parent('Príncipe William','Príncipe Jorge').
parent('Duquesa Catalina','Príncipe Jorge').

parent('Príncipe William','Princesa Carlota').
parent('Duquesa Catalina','Princesa Carlota').

parent('Príncipe William','Príncipe Luis').
parent('Duquesa Catalina','Príncipe Luis').

female('Princesa Carlota').
male('Príncipe Jorge').
male('Príncipe Luis').


parent('Duque Harry','Príncipe Archie').
parent('Duquesa Meghan','Príncipe Archie').

parent('Duque Harry','Princesa Lilibet').
parent('Duquesa Meghan','Princesa Lilibet').

female('Princesa Lilibet').
male('Príncipe Archie').


parent('Princesa Ana','Zara Phillips').
parent('Mark Phillips','Zara Phillips').

parent('Princesa Ana','Peter Phillips').
parent('Mark Phillips','Peter Phillips').

female('Zara Phillips').
male('Peter Phillips').


parent('Zara Phillips','Mia Tindall').
parent('Mike Tindall','Mia Tindall').

parent('Zara Phillips','Lena Tindall').
parent('Mike Tindall','Lena Tindall').

parent('Zara Phillips','Lucas Tindall').
parent('Mike Tindall','Lucas Tindall').

female('Mia Tindall').
female('Lena Tindall').
male('Lucas Tindall').


parent('Peter Phillips','Savannah Phillips').
parent('Autumn Kelly', 'Savannah Phillips').

parent('Peter Phillips','Isla Phillips').
parent('Autumn Kelly', 'Isla Phillips').

female('Savannah Phillips').
female('Isla Phillips').


parent('Príncipe Andrés', 'Princesa Beatriz').
parent('Duquesa Sarah', 'Princesa Beatriz').

parent('Príncipe Andrés', 'Princesa Eugenia').
parent('Duquesa Sarah', 'Princesa Eugenia').

female('Princesa Beatriz').
female('Princesa Eugenia').


parent('Princesa Beatriz','Sienna Elizabeth').
parent('Edoardo Mapelli','Sienna Elizabeth').

female('Sienna Elizabeth').


parent('Princesa Eugenia', 'August Philip').
parent('Jack Brooksbank', 'August Philip').

male('August Philip').


parent('Príncipe Eduardo','Lady Louise').
parent('Condesa Sofía','Lady Louise').

parent('Príncipe Eduardo','Vizconde James').
parent('Condesa Sofía','Vizconde James').


female('Lady Louise').
male('Vizconde James').


mother(X,Y):- parent(X,Y),female(X).
father(X,Y):- parent(X,Y),male(X).

father_report:-     
write('Known fathers are:'),nl,   
father(X),   
write(X),nl,   
fail.