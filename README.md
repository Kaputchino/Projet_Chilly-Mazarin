# Projet_Chilly-Mazarin

Déclarer une variable

input a = true|false;

input a; dans ce cas la a sera de type non déterminé, c'est à dire que le programme testera avec a = true et a =false


déclarer une gate

gate g(params input)(params outpout){

  definition outpout:
  
  output 1 = input1 + input2;
  
}


instancier une gate


g = G(A,B);



Accéder à une valeur d'une instance de gate

g.x;

g.y;



Afficher

print(params);




| symbole    | signification                                          |
|------------|--------------------------------------------------------|
| - & ∩ ∧    | AND                                                    |
| + \| ∪ ∨   | OR                                                     |
| input      | déclare une variable                                   |
| gate       | déclare une gate                                       |
| =          | assigne une valeur                                     |
| .          | permet d'accéder à une variable d'une instance de gate |
| !          | Inverse                                                |
