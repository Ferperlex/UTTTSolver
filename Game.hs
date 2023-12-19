module Main (main) where

import AI (minimax)
import Board
  ( Cell (..),
    UltimateBoard (UltimateBoard, boards, freeLargeCells),
    displayUltimateBoard,
    emptyUltimateBoard,
    updateBoard,
  )
import Logic (GameOutcome (..), validMove, winner)
import Text.Read (readMaybe)
import Types
  ( Cell (..),
    GameOutcome (..),
    UltimateBoard (UltimateBoard, boards, freeLargeCells),
  )

main :: IO ()
main = do
  putStrLn "Welcome to Ultimate Tic-Tac-Toe!"
  putStrLn "Link to the rules: https://en.wikipedia.org/wiki/Ultimate_tic-tac-toe#Rules"
  putStrLn "Numbering system for choosing moves on both small and large board:\n+-------+\n| 1 2 3 |\n| 4 5 6 |\n| 7 8 9 |\n+-------+\nTo undo the selection of a large board cell, enter 10.\nAt any moment in time, type 'Undo' to undo your previous move."
  putStrLn "AI is making a move..."
  let initialBoard = updateBoard emptyUltimateBoard (5, 5) X
  putStrLn $ displayUltimateBoard initialBoard
  putStrLn "Best move: (5,5)"
  let initialHistory = [initialBoard]
  humanTurn O initialBoard initialHistory

aiTurn :: UltimateBoard -> [UltimateBoard] -> IO ()
aiTurn board history = do
  putStrLn "AI is making a move..."
  let depth = 8
  move <- minimax board depth
  let newBoard = updateBoard board move X
  processMove X newBoard move (newBoard : history)

playerTurn :: Cell -> UltimateBoard -> [UltimateBoard] -> IO ()
playerTurn player board history
  | player == X = humanTurn player board history
  | otherwise = aiTurn board history

processMove :: Cell -> UltimateBoard -> (Int, Int) -> [UltimateBoard] -> IO ()
processMove player board move history = do
  let newBoard = updateBoard board move player
  putStrLn $ displayUltimateBoard newBoard
  putStrLn ""
  case winner newBoard of
    Win player -> printWinner (Win player)
    Draw -> printWinner Draw
    Ongoing ->
      if player == X
        then humanTurn O newBoard history
        else aiTurn newBoard history

humanTurn :: Cell -> UltimateBoard -> [UltimateBoard] -> IO ()
humanTurn player board history = do
  putStrLn $ "Player " ++ show player ++ ", it's your move!"
  putStrLn $ "Eligible large cells for your move: " ++ show (freeLargeCells board)
  promptResult <- promptMove board
  case promptResult of
    Left "Undo" -> undoMove history
    Right (Just validMove) -> processMove player board validMove (board : history) -- Update history
    Right Nothing -> putStrLn "Invalid move, please try again.\n" >> humanTurn player board history
    Left _ -> putStrLn "Invalid input, please try again." >> humanTurn player board history

undoMove :: [UltimateBoard] -> IO ()
undoMove (_ : previous : rest) = do
  putStrLn "Undoing the last move..."
  putStrLn $ displayUltimateBoard previous
  humanTurn O previous rest
undoMove [initialState] = do
  putStrLn "No more moves to undo or already at the initial state."
  humanTurn O initialState [initialState]
undoMove [] = error "Unexpected empty history list in undoMove." -- This should never happen if the history is managed correctly

playerOf :: UltimateBoard -> Cell
playerOf uboard@(UltimateBoard _ (-1, -1) _ _) = X -- Default to X if no moves have been made
playerOf uboard =
  let totalMoves = length [cell | board <- boards uboard, cell <- board, cell /= Empty]
   in if even totalMoves then O else X

promptMove :: UltimateBoard -> IO (Either String (Maybe (Int, Int)))
promptMove board = do
  putStrLn "Index of the large board cell (1-9):"
  bigBoardInput <- getLine
  if bigBoardInput == "Undo"
    then return $ Left "Undo"
    else do
      putStrLn "Index of the small board cell (1-9):"
      smallBoardInput <- getLine
      if smallBoardInput == "Undo"
        then return $ Left "Undo"
        else case (readMaybe bigBoardInput, readMaybe smallBoardInput) of
          (Just bigBoard, Just smallBoard) ->
            return $ Right $ if validMove (bigBoard, smallBoard) board then Just (bigBoard, smallBoard) else Nothing
          _ -> return $ Right Nothing

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