module AI where

import Board
import Control.Monad (forM, forM_, replicateM_)
import Data.Function (on)
import Data.Functor qualified
import Data.IORef
-- import Data.List (maximumBy)
import Data.Maybe
import Debug.Trace 
import Logic (checkWin, gameIsOver, winner, nextPlayer, currentBoardIndex)
import System.Random (randomRIO)
import Types
import Data.List (maximumBy, minimumBy)
import Data.Ord (comparing)



data MinimaxNode = MinimaxNode {
  gameState :: UltimateBoard,
  move :: Maybe (Int, Int),
  children :: [MinimaxNode]
}


minimax :: UltimateBoard -> Int -> IO (Int, Int)
minimax board depth = do
  traceIO "Starting Minimax..."
  let rootNode = MinimaxNode { gameState = board, move = Nothing, children = [] }
  traceIO "DEBUG A"
  let bestMoveResult = bestMove rootNode depth X True
  traceIO $ "Best move: " ++ show bestMoveResult -- code does not reach here!
  traceIO $ "Minimax returned: " ++ show bestMoveResult  
  return bestMoveResult


bestMove :: MinimaxNode -> Int -> Cell -> Bool -> (Int, Int)
bestMove node depth player isMaximizingPlayer = 
  let moves = trace "DEBUG B" $ possibleMoves (gameState node)
      scoredMoves = map (\m -> (m, minimaxValue (updateBoard (gameState node) m player) (depth - 1) (nextPlayer player) (not isMaximizingPlayer))) moves
  in fst $ if isMaximizingPlayer then maximumBy (comparing snd) scoredMoves else minimumBy (comparing snd) scoredMoves


minimaxValue :: UltimateBoard -> Int -> Cell -> Bool -> Int
minimaxValue board depth player isMaximizingPlayer =
  trace "DEBUG C" $  -- Debug print statement
    if depth == 0 || gameIsOver board
    then evaluate board  -- No need to round here
    else if isMaximizingPlayer
         then maximum [minimaxValue (updateBoard board move player) (depth - 1) (nextPlayer player) False | move <- possibleMoves board]
         else minimum [minimaxValue (updateBoard board move player) (depth - 1) (nextPlayer player) True | move <- possibleMoves board]




evaluate :: UltimateBoard -> Int
evaluate board =
  trace "DEBUG D" $
  let currentIndex = currentBoardIndex board
  in case winner board of
       Win X -> -10
       Win O -> 10
       Draw  -> 0
       Ongoing -> round (evaluateGame board currentIndex)  -- Round the result of evaluateGame




evaluateGame :: UltimateBoard -> Int -> Double
evaluateGame (UltimateBoard boards _ _ _) currentBoard =
  trace "DEBUG E" $
    let evaluatorMul = [1.4, 1.0, 1.4, 1.0, 1.75, 1.0, 1.4, 1.0, 1.4]
        evals = trace "DEBUG evals" $ zipWith (\board mul -> realEvaluateSquare (boardToInts board) * 1.5 * mul - fromIntegral (checkWinCondition (boardToInts board)) * mul) boards evaluatorMul
        currentBoardEval = trace "DEBUG currentBoardEval" $ realEvaluateSquare (boardToInts (boards !! currentBoard)) * evaluatorMul !! currentBoard
        mainBd = trace "DEBUG mainBd" $ map (fromIntegral . checkWinCondition . boardToInts) boards -- problematic line
        mainBdEval = trace "DEBUG mainBdEval" $ realEvaluateSquare mainBd * 150
        winConditionEval = trace "DEBUG winConditionEval" $ fromIntegral (checkWinCondition mainBd) * (-5000)
        result = sum evals + currentBoardEval + mainBdEval + winConditionEval
    in trace ("DEBUG F: " ++ show result) result






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
      twoInLineIndices = [(0, 1, 2, 2), (1, 2, 0, 0), (0, 2, 1, 1),
                          (3, 4, 5, 5), (4, 5, 3, 3), (3, 5, 4, 4),
                          (6, 7, 8, 8), (7, 8, 6, 6), (6, 8, 7, 7),
                          (0, 3, 6, 6), (3, 6, 0, 0), (0, 6, 3, 3),
                          (1, 4, 7, 7), (4, 7, 1, 1), (1, 7, 4, 4),
                          (2, 5, 8, 8), (5, 8, 2, 2), (2, 8, 5, 5),
                          (0, 4, 8, 8), (4, 8, 0, 0), (0, 8, 4, 4),
                          (2, 4, 6, 6), (4, 6, 2, 2), (2, 6, 4, 4)]
      sumLine i j k = pos !! i + pos !! j + pos !! k
  in evaluation - lineSum 1 - lineSumDiag 1 + twoInLine (-1) + lineSum (-1) + lineSumDiag (-1) - twoInLine 1


-- THis is where the error is mostly likely to be
checkWinCondition :: [Int] -> Int
checkWinCondition board
  | length board < 9 = error "Board size is less than 9"  -- or handle this case appropriately
  | otherwise =
      let check a = any (== a * 3) [sumLine 0 1 2, sumLine 3 4 5, sumLine 6 7 8, sumLine 0 3 6, sumLine 1 4 7, sumLine 2 5 8, sumLine 0 4 8, sumLine 2 4 6]
          sumLine i j k = board !! i + board !! j + board !! k
      in if check 1 then 1 else if check (-1) then -1 else 0






