module AI where

import Board
import Control.Monad (forM, forM_, replicateM_)
import Data.Function (on)
import Data.Functor qualified
import Data.IORef
import Data.List (maximumBy)
import Data.Maybe
import Debug.Trace
import Logic
import System.Random (randomRIO)
import Types

data MCTSNode = MCTSNode
  { gameState :: UltimateBoard,
    parent :: Maybe (IORef MCTSNode),
    children :: [IORef MCTSNode], -- Now a list of IORefs
    wins :: Int,
    simulations :: Int
  }

data MinimaxNode = MinimaxNode {
  gameState :: UltimateBoard,
  move :: Maybe (Int, Int),
  children :: [MinimaxNode]
}

minimax :: UltimateBoard -> Int -> IO (Int, Int)
minimax board depth = do
  traceIO "Starting Minimax..."
  let rootNode = MinimaxNode { gameState = board, move = Nothing, children = [] }
  let bestMoveResult = bestMove rootNode depth X True
  traceIO $ "Best move: " ++ show bestMoveResult
  return bestMoveResult

bestMove :: MinimaxNode -> Int -> Cell -> Bool -> (Int, Int)
bestMove node depth player isMaximizingPlayer = 
  let moves = getPossibleMoves (gameState node)
      scoredMoves = map (\m -> (m, minimaxValue (updateBoard (gameState node) m player) (depth - 1) (nextPlayer player) (not isMaximizingPlayer))) moves
  in fst $ if isMaximizingPlayer then maximumBy (comparing snd) scoredMoves else minimumBy (comparing snd) scoredMoves


minimaxValue :: UltimateBoard -> Int -> Cell -> Bool -> Int
minimaxValue board depth player isMaximizingPlayer
  | depth == 0 || gameIsOver board = evaluate board
  | isMaximizingPlayer = maximum [minimaxValue (updateBoard board move player) (depth - 1) (nextPlayer player) False | move <- getPossibleMoves board]
  | otherwise = minimum [minimaxValue (updateBoard board move player) (depth - 1) (nextPlayer player) True | move <- getPossibleMoves board]


evaluate :: UltimateBoard -> Int
evaluate board =
  case winner board of
    Win X -> 10   -- Assuming AI is X and a win is positive
    Win O -> -10  -- Assuming AI is X and a loss is negative
    Draw  -> 0    -- A draw might be considered neutral
    Ongoing -> evaluateBoardState board  -- For non-terminal states


mcts :: UltimateBoard -> Int -> IO (Int, Int)
mcts board numIterations = do
  traceIO "Starting MCTS..."
  rootNodeRef <- newIORef MCTSNode {gameState = board, parent = Nothing, children = [], wins = 0, simulations = 0}

  replicateM_ numIterations $ do
    promisingNodeRef <- selectPromisingNode rootNodeRef
    expandNode promisingNodeRef
    childNodesRefs <- readIORef promisingNodeRef >>= \node -> return (children node)

    forM_ childNodesRefs $ \childNodeRef -> do
      childNode <- readIORef childNodeRef
      outcome <- simulateRandomPlayout childNode
      backpropagation childNodeRef outcome
  rootNode <- readIORef rootNodeRef
  (bestMoveResult, winChance) <- bestMove rootNode
  traceIO $ "Best move: " ++ show bestMoveResult ++ ", Winning chance: " ++ show winChance
  return bestMoveResult

simulateRandomPlayout :: MCTSNode -> IO GameOutcome
simulateRandomPlayout node = do
  let board = gameState node
  if isTerminal board
    then return $ winner board
    else do
      randomMove <- getRandomMove board
      let newBoard = updateBoard board randomMove (getNextPlayer board)
      simulateRandomPlayout
        MCTSNode
          { gameState = newBoard,
            parent = Nothing,
            children = [],
            wins = 0,
            simulations = 0
          }

isTerminal :: UltimateBoard -> Bool
isTerminal board = case winner board of
  Ongoing -> False
  _ -> True

getRandomMove :: UltimateBoard -> IO (Int, Int)
getRandomMove board = do
  let moves = possibleMoves board
  index <- randomRIO (0, length moves - 1)
  return (moves !! index)

getNextPlayer :: UltimateBoard -> Cell
getNextPlayer ultimateBoard@(UltimateBoard _ (lastBig, lastSmall) _ _)
  | lastBig == -1 = X
  | otherwise = alternatePlayer $ cellAt (lastBig, lastSmall) ultimateBoard
  where
    alternatePlayer X = O
    alternatePlayer O = X

expandNode :: IORef MCTSNode -> IO ()
expandNode nodeRef = do
  node <- readIORef nodeRef
  let currentBoard = gameState node
  let currentPlayer = getNextPlayer currentBoard
  let possibleMovesList = possibleMoves currentBoard

  childNodesRefs <- forM possibleMovesList $ \move -> do
    let newBoard = updateBoard currentBoard move currentPlayer
    newIORef
      MCTSNode
        { gameState = newBoard,
          parent = Just nodeRef,
          children = [],
          wins = 0,
          simulations = 0
        }
  writeIORef nodeRef $ node {children = childNodesRefs}

createChildNode :: UltimateBoard -> (Int, Int) -> Cell -> IORef MCTSNode -> MCTSNode
createChildNode board move player parentRef =
  let newBoard = updateBoard board move player
   in MCTSNode
        { gameState = newBoard,
          parent = Just parentRef,
          children = [],
          wins = 0,
          simulations = 0
        }

selectPromisingNode :: IORef MCTSNode -> IO (IORef MCTSNode)
selectPromisingNode nodeRef = do
  node <- readIORef nodeRef
  if null (children node)
    then return nodeRef
    else do
      bestChildRef <- maxUCTNode (children node) >>= maybe (return nodeRef) return
      selectPromisingNode bestChildRef

maxUCTNode :: [IORef MCTSNode] -> IO (Maybe (IORef MCTSNode))
maxUCTNode [] = trace "maxUCTNode called with empty list" $ return Nothing
maxUCTNode nodeRefs = do
  nodeWithUctValues <- forM nodeRefs $ \nodeRef -> do
    uctValueResult <- uctValue nodeRef
    return (nodeRef, uctValueResult)
  return $ Just $ fst $ maximumBy (compare `on` snd) nodeWithUctValues

uctValue :: IORef MCTSNode -> IO Double
uctValue nodeRef = do
  node <- readIORef nodeRef
  parentSimulations <- case parent node of
    Just parentRef -> readIORef parentRef Data.Functor.<&> simulations
    Nothing -> return 0
  let w = fromIntegral (wins node)
  let n = fromIntegral (simulations node)
  let t = fromIntegral parentSimulations
  let c = sqrt 2 -- Exploration parameter
  return $ if n == 0 then 1 / 0 else w / n + c * sqrt (log t / n)

totalSimulations :: Maybe MCTSNode -> Int
totalSimulations Nothing = 0
totalSimulations (Just node) = simulations node

backpropagation :: IORef MCTSNode -> GameOutcome -> IO ()
backpropagation = backpropagateHelper

backpropagateHelper :: IORef MCTSNode -> GameOutcome -> IO ()
backpropagateHelper nodeRef outcome = do
  node <- readIORef nodeRef
  let newWins = if outcome == Win (playerOfNode node) then wins node + 1 else wins node
  let newNode = node {wins = newWins, simulations = simulations node + 1}
  writeIORef nodeRef newNode
  case parent newNode of
    Just parentNodeRef -> backpropagateHelper parentNodeRef outcome
    Nothing -> return ()

playerOfNode :: MCTSNode -> Cell
playerOfNode node = getNextPlayer (gameState node)

bestMove :: MCTSNode -> IO ((Int, Int), Double)
bestMove rootNode = do
  childRefsAndNodes <- mapM (\ref -> readIORef ref >>= \node -> return (ref, node)) (children rootNode)
  let (bestChildRef, bestChildNode) = maximumBy (compare `on` (simulations . snd)) childRefsAndNodes

  let winChance =
        if simulations bestChildNode > 0
          then fromIntegral (wins bestChildNode) / fromIntegral (simulations bestChildNode)
          else 0
  move <- extractMoveFromNode bestChildRef
  return (move, winChance)

extractMoveFromNode :: IORef MCTSNode -> IO (Int, Int)
extractMoveFromNode nodeRef = do
  node <- readIORef nodeRef
  case parent node of
    Just parentRef -> do
      parentNode <- readIORef parentRef
      return $ findDifference (gameState parentNode) (gameState node)
    Nothing -> error "Root node does not have a move associated with it."

findDifference :: UltimateBoard -> UltimateBoard -> (Int, Int)
findDifference oldBoard newBoard = findDiffRec (zip oldPositions newPositions)
  where
    oldPositions = concatMap (boardPositions oldBoard) [1 .. 9]
    newPositions = concatMap (boardPositions newBoard) [1 .. 9]

    findDiffRec :: [(((Int, Int), Cell), ((Int, Int), Cell))] -> (Int, Int)
    findDiffRec [] = error "No difference found between boards."
    findDiffRec (((pos, oldCell), (_, newCell)) : rest)
      | oldCell /= newCell = pos
      | otherwise = findDiffRec rest

boardPositions :: UltimateBoard -> Int -> [((Int, Int), Cell)]
boardPositions (UltimateBoard boards _ _ _) big =
  let board = boards !! (big - 1)
   in [((big, small), board !! (small - 1)) | small <- [1 .. 9]]
