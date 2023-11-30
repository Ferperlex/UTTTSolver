module Board
  ( UltimateBoard (..),
    Cell (..),
    Board,
    emptyUltimateBoard,
    displayUltimateBoard,
    updateBoard,
  )
where

import Data.List (intercalate, intersperse)
import Logic
import Types

emptyBoard :: Board
emptyBoard = replicate 9 Empty

emptyUltimateBoard :: UltimateBoard
emptyUltimateBoard = UltimateBoard {boards = replicate 9 emptyBoard, lastMove = (-1, -1), freeLargeCells = [1 .. 9]}

cellToChar :: Cell -> Char
cellToChar Empty = '.'
cellToChar X = 'X'
cellToChar O = 'O'

displayRow :: Board -> Int -> String
displayRow board rowNum =
  let startIndex = rowNum * 3
      rowCells = take 3 . drop startIndex $ board
   in unwords . map (return . cellToChar) $ rowCells

displayBoard :: Board -> String
displayBoard board = intercalate "\n" $ map (displayRow board) [0, 1, 2]

displayBoardRow :: [Board] -> Int -> String
displayBoardRow boards rowNum = "| " ++ intercalate " | " (map (`displayRow` rowNum) boards) ++ " |"

displayUltimateBoard :: UltimateBoard -> String
displayUltimateBoard (UltimateBoard boards _ _) =
  let boardGroups = chunksOf 3 boards
      displayGroup group = intercalate "\n" $ map (displayBoardRow group) [0, 1, 2]
      separator = "\n+-------+-------+-------+\n"
      ultimateBoardStr = intercalate separator $ map displayGroup boardGroups
   in separator ++ ultimateBoardStr ++ separator

updateBoard :: UltimateBoard -> (Int, Int) -> Cell -> UltimateBoard
updateBoard (UltimateBoard bs lm freeCells) (big, small) cell =
  let boardIndex = big - 1
      cellIndex = small - 1
      updatedBoard = updateSingleBoard (bs !! boardIndex) cellIndex cell
      newBoards = take boardIndex bs ++ [updatedBoard] ++ drop (boardIndex + 1) bs
      newFreeCells = getEligibleLargeCells (UltimateBoard newBoards (big, small) freeCells)
   in UltimateBoard newBoards (big, small) newFreeCells

updateSingleBoard :: Board -> Int -> Cell -> Board
updateSingleBoard board cellIndex cell =
  take cellIndex board ++ [cell] ++ drop (cellIndex + 1) board

getEligibleLargeCells :: UltimateBoard -> [Int]
getEligibleLargeCells (UltimateBoard boards lastMove _) =
  case lastMove of
    (-1, -1) -> [1 .. 9]
    (_, lastSmall) ->
      if boardWon (boards !! (lastSmall - 1)) /= Ongoing
        then filter (\index -> boardWon (boards !! (index - 1)) == Ongoing) [1 .. 9]
        else [lastSmall]
