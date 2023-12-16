module AI where

import Board
import Control.Monad (forM, forM_, replicateM_)
import Data.Function (on)
import Data.Functor qualified
import Data.IORef
-- import Data.List (maximumBy)

import Data.List (maximumBy, minimumBy)
import Data.Maybe
import Data.Ord (comparing)
import Debug.Trace
import Logic (checkWin, currentBoardIndex, gameIsOver, nextPlayer, winner)
import System.Random (randomRIO)
import Types

data MinimaxNode = MinimaxNode
  { gameState :: UltimateBoard,
    move :: Maybe (Int, Int),
    children :: [MinimaxNode]
  }

minimax :: UltimateBoard -> Int -> IO (Int, Int)
minimax board depth = do
  traceIO "Starting Minimax..."
  let rootNode = MinimaxNode {gameState = board, move = Nothing, children = []}
  let bestMoveResult = bestMove rootNode depth X True
  traceIO $ "Best move: " ++ show bestMoveResult -- code does not reach here!
  traceIO $ "Minimax returned: " ++ show bestMoveResult
  return bestMoveResult

bestMove :: MinimaxNode -> Int -> Cell -> Bool -> (Int, Int)
bestMove node depth player isMaximizingPlayer =
  let moves = possibleMoves (gameState node)
      scoredMoves =
        [ (m, eval) | m <- moves, let eval = minimaxValue (updateBoard (gameState node) m player) (depth - 1) (nextPlayer player) (not isMaximizingPlayer), trace ("Move: " ++ show m ++ " Evaluation: " ++ show eval) True
        ]
   in fst $ if isMaximizingPlayer then maximumBy (comparing snd) scoredMoves else minimumBy (comparing snd) scoredMoves

minimaxValue :: UltimateBoard -> Int -> Cell -> Bool -> Double
minimaxValue board depth player isMaximizingPlayer
  | depth == 0 || gameIsOver board = evaluate board
  | isMaximizingPlayer = maximum [minimaxValue (updateBoard board move player) (depth - 1) (nextPlayer player) False | move <- possibleMoves board]
  | otherwise = minimum [minimaxValue (updateBoard board move player) (depth - 1) (nextPlayer player) True | move <- possibleMoves board]

evaluate :: UltimateBoard -> Double
evaluate board =
  let currentIndex = currentBoardIndex board - 1
   in case winner board of
        Win X -> 100
        Win O -> -100
        Draw -> 0
        Ongoing -> evaluateGame board currentIndex

evaluateGame :: UltimateBoard -> Int -> Double
evaluateGame (UltimateBoard boards _ _ _) currentBoard =
  let evaluatorMul = [1.4, 1.0, 1.4, 1.0, 1.75, 1.0, 1.4, 1.0, 1.4]
      evals = zipWith (\board mul -> realEvaluateSquare (boardToInts board) * 1.5 * mul - fromIntegral (checkWinCondition (boardToInts board)) * mul) boards evaluatorMul
      currentBoardEval = realEvaluateSquare (boardToInts (boards !! currentBoard)) * evaluatorMul !! currentBoard
      mainBd = map (fromIntegral . checkWinCondition . boardToInts) boards
      mainBdEval = realEvaluateSquare mainBd * 150
      winConditionEval = fromIntegral (checkWinCondition mainBd) * (-5000)
      result = sum evals + currentBoardEval + mainBdEval + winConditionEval
   in result

-- Helper function to convert a board of Cells to a board of Ints
boardToInts :: Board -> [Int]
boardToInts = map cellToInt

-- Convert Cell to Int
cellToInt :: Cell -> Int
cellToInt Empty = 0
cellToInt X = 1
cellToInt O = -1

realEvaluateSquare :: [Int] -> Double
realEvaluateSquare pos =
  let points = [0.2, 0.17, 0.2, 0.17, 0.22, 0.17, 0.2, 0.17, 0.2]
      evaluation = sum $ zipWith (*) (map fromIntegral pos) points
      lineSum a = sum [if line == a * 3 then 6 else 0 | line <- lines]
      lineSumDiag a = sum [if diag == a * 3 then 7 else 0 | diag <- diagonals]
      twoInLine a = sum [if twoLine == 2 * a && oneEmpty == -a then 9 else 0 | (twoLine, oneEmpty) <- twoInLines]
      lines = [sumLine 0 1 2, sumLine 3 4 5, sumLine 6 7 8, sumLine 0 3 6, sumLine 1 4 7, sumLine 2 5 8]
      diagonals = [sumLine 0 4 8, sumLine 2 4 6]
      twoInLines = [(sumLine i j k, pos !! l) | (i, j, k, l) <- twoInLineIndices]
      twoInLineIndices =
        [ (0, 1, 2, 2),
          (1, 2, 0, 0),
          (0, 2, 1, 1),
          (3, 4, 5, 5),
          (4, 5, 3, 3),
          (3, 5, 4, 4),
          (6, 7, 8, 8),
          (7, 8, 6, 6),
          (6, 8, 7, 7),
          (0, 3, 6, 6),
          (3, 6, 0, 0),
          (0, 6, 3, 3),
          (1, 4, 7, 7),
          (4, 7, 1, 1),
          (1, 7, 4, 4),
          (2, 5, 8, 8),
          (5, 8, 2, 2),
          (2, 8, 5, 5),
          (0, 4, 8, 8),
          (4, 8, 0, 0),
          (0, 8, 4, 4),
          (2, 4, 6, 6),
          (4, 6, 2, 2),
          (2, 6, 4, 4)
        ]
      sumLine i j k = pos !! i + pos !! j + pos !! k
   in evaluation - lineSum 1 - lineSumDiag 1 + twoInLine (-1) + lineSum (-1) + lineSumDiag (-1) - twoInLine 1

-- Function to check if a player has won on a given board
-- 1 represents X, -1 represents O, 0 represents no win
checkWinCondition :: [Int] -> Int
checkWinCondition board
  | checkWinFor 1 = 1
  | checkWinFor (-1) = -1
  | otherwise = 0
  where
    -- Check win for a specific player (X or O)
    checkWinFor player =
      any (all (== player)) (rows ++ cols ++ diags)
    -- Define rows, columns, and diagonals
    rows = chunksOf 3 board
    cols = transpose rows
    diags = [[head board, board !! 4, board !! 8], [board !! 2, board !! 4, board !! 6]]

-- Helper function to chunk a list into sublists of a given size
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- Helper function for transposing a 2D list (i.e., matrix)
transpose :: [[a]] -> [[a]]
transpose ([] : _) = []
transpose x = map head x : transpose (map tail x)
