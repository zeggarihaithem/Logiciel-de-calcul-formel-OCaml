Projet de PFA : Calcul Formel
=============================

Date limite de rendu : 16 mai 2023, soutenances sans doute le 17/5.

## Documents

- Le sujet : [doc/sujet.pdf](doc/sujet.pdf)
- Quelques questions-réponses : [doc/FAQ.md](doc/FAQ.md)
- Un ancien formulaire de bac : [doc/Formulaire.pdf](doc/Formulaire.pdf)

## Installation

Pour compiler les fichiers sources fournis, en plus d'OCaml nous vous recommandons
fortement l'usage des outils suivants: `make`, `menhir`, `dune`, `ocamlfind`.

Sur une Debian (ou Ubuntu >= 20.4), ces outils peuvent s'installer simplement via:
`sudo apt install make ocaml menhir dune ocaml-findlib`.

Pour les autres systèmes, `make` est disponible sur toute plateforme raisonnable de
développement, et si les autres outils ne sont pas disponibles directement vous
pourrez les installer via [opam](http://opam.ocaml.org/), un gestionnaire de paquets OCaml.

Eviter fortement Ubuntu 18.04 (ou plus ancien), qui ne propose pas
l'outil `dune` (ou pire, un binaire du même nom mais qui n'a rien à
voir avec OCaml).

## Utilisation

Une fois installé les outils nécessaires (voir la section précédente), vous pouvez
compiler en lançant `make` dans le répertoire actuel `projet`, ce qui lancera `dune build`
avec les bons arguments (voir le fichier [Makefile](Makefile) pour plus de détails).

Ensuite, le petit script fourni `run` facilite ensuite le lancement du
binaire obtenu (`dune exec` avec les bons arguments). Pour l'instant,
le code principal lit une expression algébrique sur l'entrée standard,
la transforme en donnée OCaml de type `Syntax.expr`, puis la réaffiche
(voir [src/calc.ml](src/calc.ml)). Par exemple:

```sh
% ./run
x+pi*sqrt(3)
(x+(pi*sqrt(3)))
```


# Fonctions implémentées

## Évaluer
Cette fonction évalue la valeur de l’expression sans variables.

## Simplifier
Cette fonction prend une expression et retourne une expression simplifiée.

On crée récursivement toutes les variantes possibles d’expressions simplifiées et les stocke dans Set. Set est nécessaire pour contrôler des répétitions et pour ne pas faire des boucles.
La simplification consiste à normaliser l’expression et à appliquer des règles arithmétiques et trigonométriques.

*Les règles ne traitent pas tous les cas possibles, mais la structure créée permet de les ajouter facilement.*

## Substituer
Cette fonction remplace chaque occurrence d’une variable dans l’expression par une nouvelle expression.
Elle parcourt récursivement une expression et effectue une substitution de la variable.
- on introduit l’expression à substituer 
- on introduit la variable à substituer
- on introduit l’expression de substitution
- le résultat s’affiche

## Dériver
Cette fonction permet de calculer la dérivée. On introduit l’expression et la variable par rapport à laquelle on dérive.

Le principe c’est de faire des appels récursifs pour calculer les dérivées des sous expressions et les combiner selon les règles de dérivation.

## Intégrer
Cette fonction permet de calculer une intégrale définie de l'expression sur un segment.

On applique successivement les différentes méthodes et s'arrête si la méthode a réussi à intégrer l’expression :
* recherche d’une expression dans une table des primitives et application de théorème fondamental de l'analyse;
* application des règles de linéarité et intégration récursive des opérandes obtenu;
* integration par parties;
* application de la méthode du point médian.

*Dans la fonction décrite, l'intégration par partie ne gère pas la division.*

*On n’a pas implémenté la méthode de remplacement de variable.*

## Tracer
Cette fonction permet de tracer une courbe correspondant à une expression avec une seule variable :

* on introduit l’expression à tracer;
* on introduit la variable de l’expression;
* une fenêtre de dialogue s’affiche pour choisir si on veut introduire les intervalles des axes x et y, ou bien on garde les intervalles par défaut (-5,5)(-5,5);
* si on choisit “o” (oui on veut des intervalles personnalisés), on introduira les valeurs x_min, x_max, y_min et y_max. ensuite le graph s’affiche dans ces intervalle;
* sinon, le graph s’affiche dans l’intervalle par défaut.

## Historique

Cette fonction affiche les commandes précédentes et les résultats obtenus.