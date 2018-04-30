module Deck
(
    Card(..),
    Deck(..),
    Hand(..),
    getDeck,
    cardValue,
) where

data Card = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine |
            Ten | Jack | Queen | King 
            deriving (Eq, Show)
             
type Deck = [Card]
type Hand = [Card]


{- One suit consists 13 different cards -}
cardSuit :: [Card]
cardSuit = [Ace, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King]


{- One deck consists four suits -}
getDeck :: Deck
getDeck = foldl (\accum x -> accum ++ cardSuit) [] [1,2,3,4]

{- Card values -}
cardValue :: Card -> Int
cardValue Ace = 1 -- The base value of Ace is set to 1
cardValue Two = 2
cardValue Three = 3
cardValue Four = 4
cardValue Five = 5
cardValue Six = 6
cardValue Seven = 7
cardValue Eight = 8
cardValue Nine = 9
cardValue Ten = 10
cardValue Jack = 10
cardValue Queen = 10
cardValue King = 10







