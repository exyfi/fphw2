module HW.Client where

import HW.Game
import Data.Bifunctor
import Control.Concurrent
import Control.Monad

import Brick
import Brick.BChan
import qualified Graphics.Vty as V
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C

data AppState = AppState { game :: GameState
                         , sel :: (Int, Int)
                         }

data AppEvent = SetGameState GameState

type Name = ()

app :: App AppState AppEvent Name
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const theMap
          }

initialState :: AppState
initialState = AppState { game = newGame
                        , sel = (0, 0)
                        }

clientMain :: IO ()
clientMain = do
    chan <- newBChan 16
    let buildVty = V.mkVty V.defaultConfig
    initialVty <- buildVty
    void $ customMain initialVty buildVty (Just chan) app initialState

validSel :: (Int, Int) -> Bool
validSel (x, y) = (x >= 0) && (x < 3) && (y >= 0) && (y < 3)

moveSel :: AppState -> Int -> Int -> AppState
moveSel s x y = s { sel = if validSel sel' then sel' else sel s }
    where sel' = bimap (+x) (+y) $ sel s

handleEvent :: AppState -> BrickEvent Name AppEvent -> EventM Name (Next AppState)
handleEvent s (AppEvent (SetGameState gameState))   = continue $ s { game = gameState }
handleEvent s (VtyEvent (V.EvKey V.KLeft []))       = continue $ moveSel s 0 (-1)
handleEvent s (VtyEvent (V.EvKey V.KRight []))      = continue $ moveSel s 0 1
handleEvent s (VtyEvent (V.EvKey V.KUp []))         = continue $ moveSel s (-1) 0
handleEvent s (VtyEvent (V.EvKey V.KDown []))       = continue $ moveSel s 1 0
handleEvent s (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt s
handleEvent s (VtyEvent (V.EvKey V.KEsc []))        = halt s
handleEvent s _                                     = continue s

drawUI :: AppState -> [Widget Name]
drawUI s = [ C.center $ padRight (Pad 2) (drawBoard s) <+> (drawInfo s) ]

drawBoard :: AppState -> Widget Name
drawBoard s = withBorderStyle BS.unicodeBold
    $ B.borderWithLabel (str "3x3")
    $ vBox rows
    where rows     = [hBox $ cells r | r <- [0..2]]
          cells r  = [draw r c | c <- [0..2]]
          draw r c = bord r c $ padLeftRight 1 $ drawCell $ getCellXY r c $ board $ game s
          bord r c = if (r, c) == sel s then B.border
                                        else padAll 1

drawCell :: Cell -> Widget Name
drawCell (Just X) = withAttr xAttr $ str "X"
drawCell (Just O) = withAttr oAttr $ str "O"
drawCell Nothing  = withAttr emptyAttr $ str " "

xAttr, oAttr, emptyAttr :: AttrName
xAttr = "xAttr"
oAttr = "oAttr"
emptyAttr = "emptyAttr"

drawInfo :: AppState -> Widget Name
drawInfo s = emptyWidget

theMap :: AttrMap
theMap = attrMap V.defAttr
    [ (xAttr, fg V.red `V.withStyle` V.bold)
    , (oAttr, fg V.blue `V.withStyle` V.bold)
    ]
