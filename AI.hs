module AI where

import Board
import Control.Parallel.Strategies
import Data.List (maximumBy, minimumBy)
import Data.Ord (comparing)
import Data.Time
import Debug.Trace
import Logic (currentBoardIndex, gameIsOver, nextPlayer, winner)
import Types

data MinimaxNode = MinimaxNode
  { gameState :: UltimateBoard,
    move :: Maybe (Int, Int),
    children :: [MinimaxNode]
  }

minimax :: UltimateBoard -> Int -> IO (Int, Int)
minimax board depth = do
  startTime <- getCurrentTime
  let rootNode = MinimaxNode {gameState = board, move = Nothing, children = []}
  let bestMoveResult = bestMove rootNode depth X True
  putStrLn $ "AI moves to: " ++ show bestMoveResult
  endTime <- getCurrentTime
  let timeTaken = diffUTCTime endTime startTime
  putStrLn $ "Time taken for move: " ++ show timeTaken
  return bestMoveResult

bestMove :: MinimaxNode -> Int -> Cell -> Bool -> (Int, Int)
bestMove node depth player isMaximizingPlayer =
  let moves = possibleMoves (gameState node)
      alpha = -1 / 0 -- Negative infinity
      beta = 1 / 0 -- Positive infinity
      scoredMoves = parMap rpar (evaluateMoveWithTrace alpha beta) moves
   in fst $
        if isMaximizingPlayer
          then maximumBy (comparing snd) scoredMoves
          else minimumBy (comparing snd) scoredMoves
  where
    evaluateMoveWithTrace alpha beta move =
      let eval = minimaxValue (updateBoard (gameState node) move player) (depth - 1) (nextPlayer player) (not isMaximizingPlayer) alpha beta
       in trace (moveTraceString (move, eval)) (move, eval)

    moveTraceString (m, eval) =
      "Processed Move: " ++ show m ++ " Evaluation: " ++ show eval

minimaxValue :: UltimateBoard -> Int -> Cell -> Bool -> Double -> Double -> Double
minimaxValue board depth player isMaximizingPlayer alpha beta
  | depth == 0 || gameIsOver board = evaluate board
  | isMaximizingPlayer = goMax alpha beta (possibleMoves board)
  | otherwise = goMin alpha beta (possibleMoves board)
  where
    goMax alpha beta [] = alpha
    goMax alpha beta (m : ms) =
      let newAlpha = max alpha (minimaxValue (updateBoard board m player) (depth - 1) (nextPlayer player) False alpha beta)
       in if newAlpha >= beta then newAlpha else goMax newAlpha beta ms

    goMin alpha beta [] = beta
    goMin alpha beta (m : ms) =
      let newBeta = min beta (minimaxValue (updateBoard board m player) (depth - 1) (nextPlayer player) True alpha beta)
       in if alpha >= newBeta then newBeta else goMin alpha newBeta ms

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

boardToInts :: Board -> [Int]
boardToInts = map cellToInt

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

checkWinCondition :: [Int] -> Int
checkWinCondition board
  | checkWinFor 1 = 1
  | checkWinFor (-1) = -1
  | otherwise = 0
  where
    checkWinFor player =
      any (all (== player)) (rows ++ cols ++ diags)
    rows = chunksOf 3 board
    cols = transpose rows
    diags = [[head board, board !! 4, board !! 8], [board !! 2, board !! 4, board !! 6]]

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

transpose :: [[a]] -> [[a]]
transpose ([] : _) = []
transpose x = map head x : transpose (map tail x)