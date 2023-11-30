module Main (main) where

import Board
import Logic
import Text.Read (readMaybe)
import Types

main :: IO ()
main = do
  putStrLn "Welcome to Ultimate Tic-Tac-Toe!"
  putStrLn "Link to the rules:"
  putStrLn "https://en.wikipedia.org/wiki/Ultimate_tic-tac-toe#Rules"
  putStrLn "Numbering system for choosing moves on both small and large board:\n+-------+\n| 1 2 3 |\n| 4 5 6 |\n| 7 8 9 |\n+-------+\nTo undo the selection of a large board cell, enter 10 as your small board cell."
  putStrLn $ displayUltimateBoard emptyUltimateBoard
  playerTurn X emptyUltimateBoard

playerTurn :: Cell -> UltimateBoard -> IO ()
playerTurn player board = do
  putStrLn $ "Player " ++ show player ++ ", it's your move!"
  putStrLn $ "Eligible large cells for your move: " ++ show (freeLargeCells board)
  move <- promptMove board
  case move of
    Just validMove -> processMove player board validMove
    Nothing -> putStrLn "Invalid move, please try again." >> playerTurn player board

processMove :: Cell -> UltimateBoard -> (Int, Int) -> IO ()
processMove player board move = do
  let newBoard = updateBoard board move player
  putStrLn $ displayUltimateBoard newBoard
  putStrLn ""
  case winner newBoard of
    Win player -> printWinner (Win player)
    Draw -> printWinner Draw
    Ongoing -> playerTurn (nextPlayer player) newBoard

promptMove :: UltimateBoard -> IO (Maybe (Int, Int))
promptMove board = do
  putStrLn "Enter the index of the large board cell (1-9):"
  bigBoard <- getValidInput
  putStrLn "Enter the index of the small board cell (1-9):"
  smallBoard <- getValidInput
  let move = (bigBoard, smallBoard)
  return $ if validMove move board then Just move else Nothing

getValidInput :: IO Int
getValidInput = do
  input <- getLine
  case readMaybe input of
    Just value -> return value
    Nothing -> putStrLn "Invalid input, please enter a number." >> getValidInput

nextPlayer :: Cell -> Cell
nextPlayer X = O
nextPlayer O = X

printWinner :: GameOutcome -> IO ()
printWinner outcome = case outcome of
  Win X -> putStrLn "Player X wins!"
  Win O -> putStrLn "Player O wins!"
  Draw -> putStrLn "It's a tie!"
  _ -> putStrLn "Game is still ongoing."
