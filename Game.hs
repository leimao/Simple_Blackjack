{- Basic Game Functionalities -}

module Game
(
    Player(..),
    GameStatus(..),
    Game(..),
    initGame,
    softHand,
    hardScore,
    softScore,
    maxValidScore,
    cardCounter,
    dealerTurn,
    playerTurn,
    gameOver,
    winner
) where

import Deck

data Player = Player Hand
    deriving (Show)

data GameStatus = PlayerTurn | DealerTurn | GameOver | PlayerStayed
    deriving (Eq, Show)

data Game = Game {user :: Player,  dealer :: Player, status :: GameStatus, deck :: Deck}
    deriving (Show)


{- Initialize game: distribute two cards from deck to each player -}
initGame :: Deck -> Game
initGame deck@(c1:c2:c3:c4:rest) = Game {user = Player [c1, c2], dealer = Player [c3, c4], status = PlayerTurn, deck = rest}

{- Check if Ace is in hand -}
softHand :: Hand -> Bool
softHand hand = (elem Ace hand)

{- The value of Ace is 1 -}
hardScore :: Hand -> Int
hardScore hand = foldl (\accum x -> accum + (cardValue x)) 0 hand

{- The value of Ace is 11 -}
{- If the player has two Aces, only one will be counted as 11 -}
softScore :: Hand -> Int
softScore hand = if (softHand hand == True) then foldl (\accum x -> accum + (cardValue x)) 10 hand else (hardScore hand)

{- Max valid value -}
maxValidScore ::  Hand -> Int
maxValidScore hand = if soft > 21 then hard else soft where
    soft = softScore hand
    hard = hardScore hand


{- Count the card at hand or in deck -}
cardCounter :: [Card] -> Int
cardCounter cards = foldl (\accum x -> accum + 1) 0 cards

{- Count the Aces at hand or in deck -}
aceCounter :: [Card] -> Int
aceCounter cards = foldl (\accum x -> if x == Ace then accum + 1 else accum) 0 cards

{- Check if hand is black jack -}
{- Black jack: The value of the hand (two cards) at the beginning of the game adds up to 21. -}
blackJack :: Hand -> Bool
blackJack hand = (cardCounter hand == 2) && (softScore hand == 21)


{- Check if a hand is bust -}
bust :: Hand -> Bool
bust hand = hardScore hand > 21


soft17 :: Hand -> Bool
soft17 hand = (maxValidScore hand == 17) && (aceCounter hand == 1) && (hardScore hand == 7) 

{- deal card to user or dealer -}
dealCard :: Game -> Game
dealCard game@(Game {user = Player userhand, dealer = Player dealerhand, status = gstatus, deck = (x:xs)})
    | gstatus == DealerTurn = (Game {user = Player userhand, dealer = Player (dealerhand ++ [x]), status = gstatus, deck = xs})
    | gstatus == PlayerTurn = (Game {user = Player ([x] ++ userhand), dealer = Player dealerhand, status = gstatus, deck = xs})


{- AI determine if dealer hits -}
{- Deterministic -}
dealerHits :: Game -> Bool
dealerHits (Game {user = Player userhand,  dealer = Player dealerhand, status = DealerTurn, deck = _}) = (maxValidScore dealerhand < 17) || (soft17 dealerhand)

{- Dealer turn of play -}
dealerTurn :: Game -> Game
dealerTurn game@(Game {user = Player userhand, dealer = Player dealerhand, status = DealerTurn, deck = gdeck}) = if dealerHits game then dealCard game
    else (Game {user = Player userhand, dealer = Player dealerhand, status = GameOver, deck = gdeck})

{- Player turn of play -}
playerTurn :: Game -> Int -> Game
playerTurn game@(Game {user = Player userhand, dealer = Player dealerhand, status = PlayerTurn, deck = gdeck}) mode
    | mode == 1 = dealCard game -- User hits
    | otherwise = (Game {user = Player userhand, dealer = Player dealerhand, status = PlayerStayed, deck = gdeck})

{- Check if deal or player has busted after each turn resulting in game over -}
gameOver :: Game -> Game
gameOver game@(Game {user = Player userhand,  dealer = Player dealerhand, status = gstatus, deck = gdeck})
    | blackJack userhand = (Game {user = Player userhand,  dealer = Player dealerhand, status = GameOver, deck = gdeck})
    | blackJack dealerhand && gstatus == DealerTurn = (Game {user = Player userhand,  dealer = Player dealerhand, status = GameOver, deck = gdeck})
    | bust userhand || bust dealerhand = (Game {user = Player userhand,  dealer = Player dealerhand, status = GameOver, deck = gdeck})
    | otherwise = game

{- When game is over, check the winner of the game -}
winner :: Game -> [Char]
winner (Game {user = Player userhand,  dealer = Player dealerhand, status = GameOver, deck = _})
    | bust userhand = "Dealer wins!"
    | bust dealerhand = "Player wins!"
    | blackJack userhand && blackJack dealerhand = "Draw."
    | blackJack userhand = "Player wins!"
    | blackJack dealerhand = "Dealer wins!"
    | maxValidScore userhand > maxValidScore dealerhand = "Player wins!"
    | maxValidScore userhand == maxValidScore dealerhand = "Draw."
    | maxValidScore userhand < maxValidScore dealerhand = "Dealer wins!"
    | otherwise = "Unexpected conditions!"

