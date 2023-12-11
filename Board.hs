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
emptyUltimateBoard = UltimateBoard {boards = replicate 9 emptyBoard, lastMove = (-1, -1), freeLargeCells = [1 .. 9], possibleMoves = [(big, small) | big <- [1 .. 9], small <- [1 .. 9]]}

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
displayUltimateBoard (UltimateBoard boards _ _ _) =
  let boardGroups = chunksOf 3 boards
      displayGroup group = intercalate "\n" $ map (displayBoardRow group) [0, 1, 2]
      separator = "\n+-------+-------+-------+\n"
      ultimateBoardStr = intercalate separator $ map displayGroup boardGroups
   in separator ++ ultimateBoardStr ++ separator

updateBoard :: UltimateBoard -> (Int, Int) -> Cell -> UltimateBoard
updateBoard (UltimateBoard bs lm freeCells possibleMoves) (big, small) cell =
  let boardIndex = big - 1
      cellIndex = small - 1
      updatedBoard = updateSingleBoard (bs !! boardIndex) cellIndex cell
      newBoards = take boardIndex bs ++ [updatedBoard] ++ drop (boardIndex + 1) bs
      newFreeCells = getEligibleLargeCells (UltimateBoard newBoards (big, small) freeCells possibleMoves)
      newPossibleMoves = getPossibleMoves (UltimateBoard newBoards (big, small) newFreeCells possibleMoves)
   in UltimateBoard newBoards (big, small) newFreeCells newPossibleMoves

updateSingleBoard :: Board -> Int -> Cell -> Board
updateSingleBoard board cellIndex cell =
  take cellIndex board ++ [cell] ++ drop (cellIndex + 1) board

getEligibleLargeCells :: UltimateBoard -> [Int]
getEligibleLargeCells (UltimateBoard boards lastMove _ _) =
  case lastMove of
    (-1, -1) -> [1 .. 9]
    (_, lastSmall) ->
      if boardWon (boards !! (lastSmall - 1)) /= Ongoing
        then filter (\index -> boardWon (boards !! (index - 1)) == Ongoing) [1 .. 9]
        else [lastSmall]

getPossibleMoves :: UltimateBoard -> [(Int, Int)]
getPossibleMoves (UltimateBoard boards _ freeLargeCells _) =
  concatMap getMovesForBoard freeLargeCells
  where
    getMovesForBoard big =
      let board = boards !! (big - 1)
          smallMoves = [small | small <- [1 .. 9], board !! (small - 1) == Empty]
       in [(big, small) | small <- smallMoves]
