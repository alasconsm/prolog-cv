/* Padres*/

parent('Isabel II','Rey Carlos III').
parent('Felipe','Rey Carlos III').
parent('Isabel II','Princesa Ana').
parent('Felipe','Princesa Ana').
parent('Isabel II','Príncipe Andrés').
parent('Felipe','Príncipe Andrés').
parent('Isabel II','Príncipe Eduardo').
parent('Felipe','Príncipe Eduardo').
parent('Princesa Diana','Príncipe William').
parent('Rey Carlos III','Príncipe William').
parent('Princesa Diana','Duque Harry').
parent('Rey Carlos III','Duque Harry').
parent('Príncipe William','Príncipe Jorge').
parent('Duquesa Catalina','Príncipe Jorge').
parent('Príncipe William','Princesa Carlota').
parent('Duquesa Catalina','Princesa Carlota').
parent('Príncipe William','Príncipe Luis').
parent('Duquesa Catalina','Príncipe Luis').
parent('Duque Harry','Príncipe Archie').
parent('Duquesa Meghan','Príncipe Archie').
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

parent('Príncipe Andrés', 'Princesa Beatriz').
parent('Duquesa Sarah', 'Princesa Beatriz').
parent('Príncipe Andrés', 'Princesa Eugenia').
parent('Duquesa Sarah', 'Princesa Eugenia').
parent('Princesa Beatriz','Sienna Elizabeth').
parent('Edoardo Mapelli','Sienna Elizabeth').
parent('Princesa Eugenia', 'August Philip').
parent('Jack Brooksbank', 'August Philip').
parent('Príncipe Eduardo','Lady Louise').
parent('Condesa Sofía','Lady Louise').
parent('Príncipe Eduardo','Vizconde James').
parent('Condesa Sofía','Vizconde James').


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
male('Príncipe Andrés').
male('Príncipe Eduardo').
male('Príncipe Jorge').
male('Príncipe Luis').
male('Príncipe Archie').
male('Peter Phillips').
male('Lucas Tindall').
male('Vizconde James').
male('August Philip').
male('Príncipe William').
male('Duque Harry').


mother(X,Y):- parent(X,Y),female(X).
father(X,Y):- parent(X,Y),male(X).

father_report:-     
write('Known fathers are:'),nl,   
father(X),   
write(X),nl,   
fail.