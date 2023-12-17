module Logic
  ( validMove,
    winner,
    GameOutcome (..),
    cellAt,
    chunksOf,
    boardWon,
    checkWin,
    gameIsOver,
    nextPlayer,
    currentBoardIndex,
  )
where

import Data.List
import Types

currentBoardIndex :: UltimateBoard -> Int
currentBoardIndex (UltimateBoard _ (big, _) _ _) = big

nextPlayer :: Cell -> Cell
nextPlayer X = O
nextPlayer O = X

gameIsOver :: UltimateBoard -> Bool
gameIsOver board =
  case winner board of
    Win _ -> True
    Draw -> True
    Ongoing -> False

validMove :: (Int, Int) -> UltimateBoard -> Bool
validMove (big, small) (UltimateBoard boards _ freeCells _) =
  inBounds big
    && inBounds small
    && big `elem` freeCells
    && cellAt (big, small) (UltimateBoard boards (-1, -1) freeCells []) == Types.Empty

inBounds :: Int -> Bool
inBounds n = n >= 1 && n < 10

lastMoveToBoard :: (Int, Int) -> Int
lastMoveToBoard (-1, -1) = -1
lastMoveToBoard (_, small) = small

winner :: UltimateBoard -> GameOutcome
winner (UltimateBoard boards _ _ _) =
  let boardOutcomes = map boardWon boards
      rows = chunksOf 3 boardOutcomes
      cols = transpose rows
      diags = [map (\i -> rows !! i !! i) [0 .. 2], map (\i -> rows !! i !! (2 - i)) [0 .. 2]]
      lines = rows ++ cols ++ diags
   in if any (all (== Win X)) lines
        then Win X
        else
          if any (all (== Win O)) lines
            then Win O
            else
              if Ongoing `notElem` boardOutcomes
                then Draw
                else Ongoing

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

boardWon :: Board -> GameOutcome
boardWon board
  | checkWin X board = Win X
  | checkWin O board = Win O
  | Types.Empty `notElem` board = Draw
  | otherwise = Ongoing

checkWin :: Cell -> Board -> Bool
checkWin player board =
  or $
    [checkLine player (getRow r board) | r <- [0 .. 2]]
      ++ [checkLine player (getCol c board) | c <- [0 .. 2]]
      ++ [checkLine player (getDiag d board) | d <- [0, 1]]

checkLine :: Cell -> [Cell] -> Bool
checkLine player = all (== player)

getRow :: Int -> Board -> [Cell]
getRow rowNum board = take 3 $ drop (rowNum * 3) board

getCol :: Int -> Board -> [Cell]
getCol colNum board = [board !! (i * 3 + colNum) | i <- [0, 1, 2]]

getDiag :: Int -> Board -> [Cell]
getDiag 0 board = [board !! (i * 4) | i <- [0, 1, 2]] -- Main diagonal
getDiag 1 board = [board !! (i * 2 + 2) | i <- [0, 1, 2]] -- Off diagonal

cellAt :: (Int, Int) -> UltimateBoard -> Cell
cellAt (big, small) (UltimateBoard bs _ _ _) =
  let boardIndex = big - 1
      cellIndex = small - 1
      selectedBoard = bs !! boardIndex
   in selectedBoard !! cellIndex