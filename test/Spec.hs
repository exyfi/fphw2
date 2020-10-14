import Test.Hspec
import HW.Game
import Data.Function

exampleGame :: BoardSize -> Maybe Player -> [(Int, Int)] -> IO ()
exampleGame size win moves = do
    let b = newGame size
    let bs = scanl (\b (x, y) -> makeMove (getCellPos x y (board b)) b) b moves
    sequence_ $ flip map (init bs) $ \t -> do
        winner (board t) `shouldBe` Nothing
    winner (board $ last bs) `shouldBe` win

-- https://proofwiki.org/wiki/Definition:Dihedral_Group_D4
exampleGameSym :: BoardSize -> Maybe Player -> [(Int, Int)] -> IO ()
exampleGameSym size win moves =
    sequence_ $ flip map sym $ \t -> exampleGame size win $ map t moves
    where a (x, y) = (y, x)
          b (x, y) = (siz - 1 - x, siz - 1 - y)
          siz = boardSizeToInt size
          sym = [ id
                , a
                , a . a
                , a . a . a
                , b
                , b . a
                , b . a . a
                , b . a . a . a
                ]

main :: IO ()
main = hspec $ do
    let sizes = [Board3x3, Board4x4, Board5x5]

    describe "BoardSize -> Int" $ do
        it "works with Board3x3" $ boardSizeToInt Board3x3 `shouldBe` 3
        it "works with Board4x4" $ boardSizeToInt Board4x4 `shouldBe` 4
        it "works with Board5x5" $ boardSizeToInt Board5x5 `shouldBe` 5

    describe "newGame creates empty board" $
        sequence_ $ flip map sizes $ \size ->
            it ("works with " <> (show size)) $ do
                let g = newGame size
                let s = boardSizeToInt size
                curPlayer g `shouldBe` X
                length (possibleMoves $ board g) `shouldBe` s^2

    describe "winner is determined properly" $ do
        it "works with example game #1" $ do
            exampleGameSym Board3x3 (Just X) $
                [ (1, 1)
                , (0, 1)
                , (2, 2)
                , (0, 0)
                , (0, 2)
                , (2, 0)
                , (1, 2)
                ]

        it "works with example game #2" $ do
            exampleGameSym Board3x3 Nothing $
                [ (1, 1)
                , (0, 0)
                , (1, 2)
                , (1, 0)
                , (2, 0)
                , (0, 2)
                , (0, 1)
                , (2, 1)
                , (2, 2)
                ]

        it "works with example game #3" $ do
            exampleGameSym Board4x4 (Just O) $
                [ (1, 0)
                , (0, 0)
                , (3, 0)
                , (1, 1)
                , (2, 1)
                , (2, 2)
                , (1, 2)
                , (3, 3)
                ]
