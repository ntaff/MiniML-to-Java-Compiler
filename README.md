# Projet Types de Données et Preuves
## Utilisation

Commandes d'utilisation en Caml :

- `parse` permet de parser le programme
- `compile_prog` permet de le compiler


## Travail réalisé
- Analyse lexicale et syntaxique
- Ecriture d'un simulateur de la CAM
- Execution de la CAM en Java
- Ecriture d'un générateur de code de Caml vers Java
- Implémentation de la récursivité

## Problèmes rencontrés

Ecriture de la fonction Compile, qui est assez complexe.
Partie Java non terminée en raison d'un manque de temps dû à la remise du suje trop tardive.

## Comment ça marche ?

Compilation des fichiers :

`cd Cam && make clean && make`

Tests dans l'interpreteur Caml :

`#use "use.ml";;`

- Pour tester la fonction ackermann "ackermann_iter.ml":

`exec (NullV, (compile_prog (parse "Tests/ackermann_iter.ml")), [], []);;`
(Erreur de parsing retournée lors de l'execution de ce test)

About the authors                                                  {#about}
-----------------

This repository was written by Nicolas Taffoureau.
