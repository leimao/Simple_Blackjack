# Simple Blackjack

Lei Mao

University of Chicago

## Introduction

[Blackjack](https://en.wikipedia.org/wiki/Blackjack) is a popular gambling game in the United States. This Simple Blackjack game was written in Haskell. 

## Rules

The rules of Blackjack could be found on [Wikipedia](https://en.wikipedia.org/wiki/Blackjack) or this [video tutorial](https://www.youtube.com/watch?v=5bWpnABkU-Y).

For simplicity, we have the following specifications:

* No shoes. Just use a single deck of cards randomly shuffled.
* No splits.
* No money (namely betting)/insurance.
* Dealer must hit on soft 17.
* Every game starts with a new deck.
* Allow player to hit when the score is 21.

## Dependencies

* GHC 8.2.2
* [Random 1.1](http://hackage.haskell.org/package/random-1.1/docs/System-Random.html)

## Usage

### Installation

To install the game, run the following command in the shell:

```bash
$ ghc Blackjack.hs
```

If got error with ``System.Random`` during compile, please try to fix with:

```bash
$ cabal update
$ cabal install -p random --reinstall --force-reinstalls
```

### Start Game

To start the game, run the following command in the shell:

```bash
$ ./Blackjack
```

## Demo

```
=================================
Welcome to the game of BlackJack!
=================================

********************
Dealer's hand:
? [Two]
Dealer's Score:
? + 2
--------------------
Player's hand:
[Ace,Nine]
Player's Score:
10/20
--------------------
What would you like to do?
(1) Hit
(2) Stay
2
********************
Dealer's hand:
[Seven,Two]
Dealer's Score:
9
--------------------
Player's hand:
[Ace,Nine]
Player's Score:
10/20
********************
Dealer's hand:
[Seven,Two,Nine]
Dealer's Score:
18
--------------------
Player's hand:
[Ace,Nine]
Player's Score:
10/20
********************
Game over!
Dealer's hand:
[Seven,Two,Nine]
Dealer's Score:
18
--------------------
Player's hand:
[Ace,Nine]
Player's Score:
10/20
Player wins!
Play again?
(1) Yes
(2) No
```

