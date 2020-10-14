module HW.Game where

import GHC.Generics
import Control.Applicative
import Data.Aeson
import Data.Bifunctor
import Data.List
import Data.Maybe
import Data.Foldable
import qualified Data.Vector as V
import Data.Vector ((!), (//))

data Player = X | O
    deriving (Eq, Show, Enum, Generic)

-- Cell on the board, may be either of the players or Nothing
type Cell = Maybe Player

-- a function that takes a state and returns the player that has a winning move
type Evaluator = GameState -> Maybe Player

data BoardSize = Board3x3
               | Board4x4
               | Board5x5
               deriving (Eq, Ord, Show, Enum, Generic, Bounded)

-- game board
data Board = Board BoardSize (V.Vector Cell)
    deriving (Eq, Show, Generic)

-- position of the cell on the board
data CellPos = CellPos BoardSize Int
    deriving (Eq, Show)

-- state of the game: board and the current player
data GameState = GameState { board     :: Board
                           , curPlayer :: Player
                           }
    deriving (Eq, Show, Generic)

instance ToJSON Player
instance FromJSON Player

instance ToJSON BoardSize
instance FromJSON BoardSize

instance ToJSON Board
instance FromJSON Board

instance ToJSON GameState
instance FromJSON GameState

boardSizeToInt :: BoardSize -> Int
boardSizeToInt Board3x3 = 3
boardSizeToInt Board4x4 = 4
boardSizeToInt Board5x5 = 5

-- get size of the board
boardSize :: Board -> Int
boardSize (Board sz _) = boardSizeToInt sz

-- create an initial game state from board size
newGame :: BoardSize -> GameState
newGame sz =
    GameState { board = Board sz (V.replicate (size^2) Nothing)
              , curPlayer = X
              }
    where size = boardSizeToInt sz

-- get the opposite player
otherPlayer :: Player -> Player
otherPlayer X = O
otherPlayer O = X

-- determine the winner of the board
winner :: Board -> Maybe Player
winner (Board sz board) = asum $ map (match . fmap (board !)) coords
    where match xs@(Just x:_)
            | all (== Just x) xs = Just x
            | otherwise          = Nothing
          match _ = Nothing
          size = boardSizeToInt sz
          coords = [[size*i..size*i+size-1] | i <- [0..size-1]] <>              -- rows
                   [[i, size+i..size^2-1] | i <- [0..size-1]] <>                -- cols
                   [[0, size+1..size^2-1], [size-1, 2*(size-1)..size*(size-1)]] -- diagonals

-- determine if the board is a tie or has winner
gameOver :: Board -> Bool
gameOver board = isJust (winner board) || null (possibleMoves board)

-- get the list of possible moves on the board
possibleMoves :: Board -> [CellPos]
possibleMoves (Board sz board) = [CellPos sz i | (i, Nothing) <- V.toList $ V.indexed board]
    where size = boardSizeToInt sz

-- set cell on the board
setCell :: CellPos -> Cell -> Board -> Board 
setCell (CellPos _ pos) cell (Board sz board) = Board sz $ board // [(pos, cell)]
    where size = boardSizeToInt sz

-- get cell on the board
getCell :: CellPos -> Board -> Cell
getCell (CellPos _ pos) (Board _ board) = board ! pos

-- get CellPos from coordinates
getCellPos :: Int -> Int -> Board -> CellPos
getCellPos x y (Board sz _) = CellPos sz $ y * size + x
    where size = boardSizeToInt sz

-- make move on the board and pass the turn to the other player
makeMove :: CellPos -> GameState -> GameState
makeMove pos (GameState board curPlayer) = GameState (setCell pos (Just curPlayer) board) (otherPlayer curPlayer)

-- make the best move on the board
makeBestMove :: GameState -> GameState
makeBestMove s
  | gameOver $ board s = s
  | otherwise = makeMove (fst $ bestMove (evalDeep 2) s) s

evalDeep :: Int -> Evaluator
evalDeep 0 (GameState board _) = winner board
evalDeep n s@(GameState board _)
  | gameOver board = winner board
  | otherwise = snd $ bestMove (evalDeep (n - 1)) s

bestMove :: Evaluator -> GameState -> (CellPos, Maybe Player)
bestMove eval state@(GameState board curPlayer) = head $ winningMoves <> drawingMoves <> losingMoves
    where moves = map (\move -> (move, eval $ makeMove move state)) (possibleMoves board)
          m p = filter (\(m, p') -> p == p') moves
          winningMoves = m $ Just curPlayer
          drawingMoves = m Nothing
          losingMoves  = m $ Just $ otherPlayer curPlayer

movePos :: Int -> Int -> CellPos -> CellPos
movePos dx dy (CellPos sz pos)
  | (x >= 0) && (x < size) && (y >= 0) && (y < size) = CellPos sz npos
  | otherwise = CellPos sz pos
    where size = boardSizeToInt sz
          x = pos `mod` size + dx
          y = pos `div` size + dy
          npos = y * size + x
