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

type Cell = Maybe Player

type Evaluator = GameState -> Maybe Player

data BoardSize = Board3x3
               | Board4x4
               | Board5x5
               deriving (Eq, Ord, Show, Enum, Generic, Bounded)

data Board = Board BoardSize (V.Vector Cell)
    deriving (Eq, Show, Generic)

data CellPos = CellPos BoardSize Int
    deriving (Eq, Show)

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

boardSize :: Board -> Int
boardSize (Board sz _) = boardSizeToInt sz

newGame :: BoardSize -> GameState
newGame sz =
    GameState { board = Board sz (V.replicate (size^2) Nothing)
              , curPlayer = X
              }
    where size = boardSizeToInt sz

otherPlayer :: Player -> Player
otherPlayer X = O
otherPlayer O = X

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

gameOver :: Board -> Bool
gameOver board = isJust (winner board) || null (possibleMoves board)

possibleMoves :: Board -> [CellPos]
possibleMoves (Board sz board) = [CellPos sz i | (i, Nothing) <- V.toList $ V.indexed board]
    where size = boardSizeToInt sz

setCell :: CellPos -> Cell -> Board -> Board 
setCell (CellPos _ pos) cell (Board sz board) = Board sz $ board // [(pos, cell)]
    where size = boardSizeToInt sz

getCell :: CellPos -> Board -> Cell
getCell (CellPos _ pos) (Board _ board) = board ! pos

getCellPos :: Int -> Int -> Board -> CellPos
getCellPos x y (Board sz _) = CellPos sz $ y * size + x
    where size = boardSizeToInt sz

makeMove :: CellPos -> GameState -> GameState
makeMove pos (GameState board curPlayer) = GameState (setCell pos (Just curPlayer) board) (otherPlayer curPlayer)

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
