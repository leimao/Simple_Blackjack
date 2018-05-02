# Makefile

all: Blackjack

Blackjack: Blackjack.hs Deck.hs Game.hs Shuffle.hs
	ghc -o Blackjack Blackjack.hs
	rm *.hi *.o

clean:
	rm Blackjack *.hi *.o
