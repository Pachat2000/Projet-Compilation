## Projet Compilateur

| Nom      | Prénom |
|----------|--------|
| Elsegahy | Rayan  |

## Lancer vos programmes

1. ocamlbuild -use-menhir main.byte
2. ./main.byte <VotreFichier.test> > main.mips
3. spim -file main.mips 

 
## Fonctionnalités

# Fonction

- Type_de_retour identifiant ( liste arguement(Type_de_retour identifiant) ) { Block d'instruction }

# Instructions

- while(condition){  Block d'instruction  }

- if(condition){ Block d'instruction } else { Block d'instruction }

- if(condition){ Block d'instruction }

- return expresion;

- type_de_variable identifiant;

- identifiant = expression;

- type_de_variable identifiant = expression;

# Expressions

- Valeur (Int, Bool, Void, Str)

- Variable identifiant

- Appelle de fonction identifiant( liste arguement )


# Valeurs

- Int -> 1212, par exemple.

- Bool -> true | false

- Void -> void

- Str -> "Hello World !"

## Opérateurs de comparaison

- == (seuleument pour les int)

- >= Supérieur ou égale

- != Différent de

- <= Inférieur ou égale

- && Et

- || Ou

## Opérations

- |expression| absolue

- variable++ incrémenter de 1

- += plus égale

- + Additioner

- variable-- décrementer de 1

- -= moin égale

- - Soustraire

- * Multiplier

- \ Diviser

- ! Not (je ne sais pas pourquoi il ne fonctionne pas)

## Affichage etc

- geti() Permet de récuperer un int par l'utilisateur

- puti(Int) Affiche un int

- puts(Str) Affiche un string
