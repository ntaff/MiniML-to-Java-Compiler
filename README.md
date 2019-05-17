# Projet Types de Données et Preuves
## Utilisation

Commandes d'utilisation en Caml :

- `parse` permet de parser le programme
- `compile_prog` permet de le compiler

## Comment ça marche ?

Compilation des fichiers :

`cd Cam && make clean && make`

Tests dans l'interpreteur Caml :

`#use "use.ml";;`

- Pour tester la fonction ackermann "ackermann_iter.ml" :

`exec (NullV, (compile_prog (parse "Tests/ackermann_iter.ml")), [], []);;`
(Erreur de parsing retournée lors de l'execution de ce test)

About the authors                                                  {#about}
-----------------

This repository was written by Nicolas Taffoureau.
