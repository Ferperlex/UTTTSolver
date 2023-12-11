module Types
  ( Cell (..),
    Board,
    UltimateBoard (..),
    GameOutcome (..),
  )
where

data Cell = Empty | X | O deriving (Eq, Show)

type Board = [Cell]

data UltimateBoard = UltimateBoard {boards :: [Board], lastMove :: (Int, Int), freeLargeCells :: [Int], possibleMoves :: [(Int, Int)]} deriving (Show)

data GameOutcome = Win Cell | Draw | Ongoing deriving (Eq, Show)
