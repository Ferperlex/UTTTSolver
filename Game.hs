module Main (main) where

import AI
import Board
import Logic
import Text.Read (readMaybe)
import Types

main :: IO ()
main = do
  putStrLn "Welcome to Ultimate Tic-Tac-Toe!"
  putStrLn "Link to the rules: https://en.wikipedia.org/wiki/Ultimate_tic-tac-toe#Rules"
  putStrLn "Numbering system for choosing moves on both small and large board:\n+-------+\n| 1 2 3 |\n| 4 5 6 |\n| 7 8 9 |\n+-------+\nTo undo the selection of a large board cell, enter 10."
  putStrLn "Do you want to go first? (Y/N)"
  firstPlayer <- getFirstPlayerChoice

  putStrLn $ displayUltimateBoard emptyUltimateBoard
  if firstPlayer
    then humanTurn O emptyUltimateBoard -- Player as O
    else aiTurn emptyUltimateBoard -- AI as X

getFirstPlayerChoice :: IO Bool
getFirstPlayerChoice = do
  choice <- getLine
  case choice of
    "Y" -> return True
    "N" -> return False
    _ -> putStrLn "Invalid input, please enter 'Y' or 'N'." >> getFirstPlayerChoice

playerTurn :: Cell -> UltimateBoard -> IO ()
playerTurn player board
  | player == X = humanTurn player board -- Human as X
  | otherwise = aiTurn board -- AI as O

aiTurn :: UltimateBoard -> IO ()
aiTurn board = do
  putStrLn "AI is making a move..."
  let depth = 7 -- Edit this to indicate how long you want the bot to take
  move <- minimax board depth
  processMove X board move -- AI as X

processMove :: Cell -> UltimateBoard -> (Int, Int) -> IO ()
processMove player board move = do
  let newBoard = updateBoard board move player
  putStrLn $ displayUltimateBoard newBoard
  putStrLn ""
  case winner newBoard of
    Win player -> printWinner (Win player)
    Draw -> printWinner Draw
    Ongoing ->
      if player == X
        then humanTurn O newBoard -- Next turn is human's as O
        else aiTurn newBoard -- Next turn is AI's as X

humanTurn :: Cell -> UltimateBoard -> IO ()
humanTurn player board = do
  putStrLn $ "Player " ++ show player ++ ", it's your move!"
  putStrLn $ "Eligible large cells for your move: " ++ show (freeLargeCells board)
  move <- promptMove board
  case move of
    Just validMove -> processMove player board validMove
    Nothing -> putStrLn "Invalid move, please try again.\n" >> humanTurn player board

promptMove :: UltimateBoard -> IO (Maybe (Int, Int))
promptMove board = do
  putStrLn "Index of the large board cell (1-9):"
  bigBoard <- getValidInput
  putStrLn "Index of the small board cell (1-9):"
  smallBoard <- getValidInput
  let move = (bigBoard, smallBoard)
  return $ if validMove move board then Just move else Nothing

getValidInput :: IO Int
getValidInput = do
  input <- getLine
  case readMaybe input of
    Just value -> return value
    Nothing -> putStrLn "Invalid input, please enter a number." >> getValidInput

printWinner :: GameOutcome -> IO ()
printWinner outcome = case outcome of
  Win X -> putStrLn "Player X wins!"
  Win O -> putStrLn "Player O wins!"
  Draw -> putStrLn "It's a tie!"
  _ -> putStrLn "Game is still ongoing."
