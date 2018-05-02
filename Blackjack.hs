{- 
Blackjack

Lei Mao
University of Chicago
-}

import Deck
import Game
import Shuffle

{- Hit or stay choice for user -}
hitOrStay :: IO Int
hitOrStay = do 
    putStrLn "What would you like to do?"
    putStrLn "(1) Hit"
    putStrLn "(2) Stay"
    optStr <- getLine 
    let optionInt = read optStr :: Int 
    return optionInt 

{- Play again choice for user -}
playAgain :: IO Int
playAgain = do 
    putStrLn "Play again?"
    putStrLn "(1) Yes"
    putStrLn "(2) No"
    optStr <- getLine
    let optionInt = read optStr :: Int 
    return optionInt 

{- Show cards along with scores -}
showCards :: Game -> IO ()
showCards game@(Game {user = Player userhand, dealer = Player dealerhand, status = gstatus, deck = gdeck}) = do
    putStrLn "Dealer's hand:"
    showDealerHand dealerhand gstatus
    putStrLn "Dealer's score:"
    showDealerScore dealerhand gstatus
    putStrLn "--------------------"
    putStrLn "Player's hand:"
    showHand userhand
    putStrLn "Player's score:"
    showScore userhand

{- Show all the cards in hand -}
showHand :: Hand -> IO ()
showHand hand = do
    putStrLn (show hand)

{- Show the scores of the cards in hand -}
showScore :: Hand ->IO ()
showScore hand
    | (softHand hand) && (softScore hand <= 21) = do 
        putStrLn ((show (hardScore hand)) ++ "/" ++ (show (softScore hand)))
    | otherwise = do
        putStrLn (show (hardScore hand))

{- Show dealer's cards in hand -}
showDealerHand :: Hand -> GameStatus -> IO ()
showDealerHand hand@(x:xs) gstatus = do
    if gstatus == PlayerTurn
        then putStrLn ("? " ++ show xs)
    else
        showHand hand

{- Show the scores of dealer's cards in hand -}
showDealerScore :: Hand -> GameStatus -> IO ()
showDealerScore hand@(x:xs) gstatus
    | (softHand xs) && (softScore xs <= 21) && gstatus == PlayerTurn = do
        putStrLn ("? + " ++ (show (hardScore xs)) ++ "/" ++ (show (softScore xs)))
    | gstatus == PlayerTurn = do
        putStrLn ("? + " ++ show (hardScore xs))
    | otherwise = do
        showScore hand

{- Game loop for single round of game -}
gameLoop :: Game -> IO () 
gameLoop gameState@(Game {user = Player userhand, dealer = Player dealerhand, status = gstatus, deck = gdeck})
    | gstatus == PlayerTurn = do 
        putStrLn "********************"
        showCards gameState
        putStrLn "--------------------"
        playerDecision <- hitOrStay
        let newGameState = gameOver $ playerTurn gameState playerDecision
        gameLoop newGameState
    | gstatus == DealerTurn = do
        putStrLn "********************"
        showCards gameState
        let newGameState = gameOver $ dealerTurn gameState
        gameLoop newGameState
    | gstatus == PlayerStayed = do
        let newGameState = (Game {user = Player userhand, dealer = Player dealerhand, status = DealerTurn, deck = gdeck})
        gameLoop newGameState
    | gstatus == GameOver = do
        putStrLn "********************"
        putStrLn "Game over!"
        showCards gameState
        putStrLn $ winner gameState
        playerChoice <- playAgain
        if playerChoice == 1 then do main else return ()

{- Main -}
main :: IO () 
main = do 
    {-
    putStrLn ""
    putStrLn "================================="
    putStrLn "Welcome to the game of BlackJack!"
    putStrLn "================================="
    putStrLn ""
    -}

    {- ASCII art modified from https://github.com/werdlerk/blackjack-console/blob/master/blackjack-oop/blackjack_view.rb -}
    putStrLn ""
    putStrLn ("+" ++ "----------------------------------------------" ++ "+")
    putStrLn ("|" ++ "  ____  _            _     _            _     " ++ "|")
    putStrLn ("|" ++ " | __ )| | __ _  ___| | __(_) __ _  ___| | __ " ++ "|")
    putStrLn ("|" ++ " |  _ \\| |/ _` |/ __| |/ /| |/ _` |/ __| |/ / " ++ "|")
    putStrLn ("|" ++ " | |_) | | (_| | (__|   < | | (_| | (__|   <  " ++ "|")
    putStrLn ("|" ++ " |____/|_|\\__,_|\\___|_|\\_\\/ |\\__,_|\\___|_|\\_\\ " ++ "|")
    putStrLn ("|" ++ "                        |__/                  " ++ "|")
    putStrLn ("+" ++ "----------------------------------------------" ++ "+")
    putStrLn ""

    deck <- shuffle getDeck
    let game = initGame deck 
    gameLoop game

