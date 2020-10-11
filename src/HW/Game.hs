module HW.Game where

import Control.Applicative
import Data.List
import Data.Maybe
import Data.Foldable

data Player = X | O
    deriving (Eq, Show, Enum)

type Cell = Maybe Player

newtype Board = Board [Cell]

data GameState = GameState { board     :: Board
                           , curPlayer :: Player
                           }

newGame :: GameState
newGame = GameState { board = Board (replicate 9 Nothing)
                    , curPlayer = X
                    }

otherPlayer :: Player -> Player
otherPlayer X = O
otherPlayer O = X

winner :: Board -> Maybe Player
winner (Board board) = asum $ map (match . map (board !!)) coords
    where match [(Just a), (Just b), (Just c)]
            | a == b && b == c = Just a
            | otherwise        = Nothing
          match _ = Nothing
          coords = [ [0, 1, 2] -- first row
                   , [3, 4, 5] -- second row
                   , [6, 7, 8] -- third row
                   , [0, 3, 6] -- first column
                   , [1, 4, 7] -- second column
                   , [2, 5, 8] -- third column
                   , [0, 4, 8] -- main diagonal
                   , [2, 4, 6] -- side diagonal
                   ]

gameOver :: Board -> Bool
gameOver board = isJust (winner board) || null (possibleMoves board)

possibleMoves :: Board -> [Int]
possibleMoves (Board board) = [i | (Nothing, i) <- zip board [0..8]]

setCell :: Int -> Cell -> Board -> Board 
setCell pos cell (Board board) = Board [replace x i | (x, i) <- zip board [0..8]]
    where replace x i
            | i == pos  = cell
            | otherwise = x

getCellXY :: Int -> Int -> Board -> Cell
getCellXY x y (Board board) = board !! (3 * x + y)

makeMove :: Int -> GameState -> GameState
makeMove pos (GameState board curPlayer) = GameState (setCell pos (Just curPlayer) board) (otherPlayer curPlayer)

evaluatePosition :: GameState -> Maybe Player
evaluatePosition state@(GameState board curPlayer)
  | gameOver board = winner board
  | otherwise      = evaluatePosition $ makeMove (bestMove state) state

bestMove :: GameState -> Int
bestMove state@(GameState board curPlayer) = head $ winningMoves <> drawingMoves <> losingMoves
    where moves p = filter (\move -> evaluatePosition (makeMove move state) == p) (possibleMoves board)
          winningMoves = moves $ Just curPlayer
          drawingMoves = moves Nothing
          losingMoves  = moves $ Just $ otherPlayer curPlayer
