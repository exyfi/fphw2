module HW.Client where

import HW.Game
import HW.Server

import System.Exit
import Data.Maybe
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad
import Control.Monad.IO.Class
import Servant.Client
import Network.HTTP.Client
import Control.Lens

import Brick
import Brick.BChan
import qualified Graphics.Vty as V
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C

data Action = MakeMove GameState
            | NewGame BoardSize

type ActionChan = Chan Action

data GameScene = GameScene { _game :: GameState
                           , _sel :: CellPos
                           , _me :: Player
                           }

makeLenses ''GameScene

data SetupScene = SetupScene { _selSize :: BoardSize }

makeLenses ''SetupScene

data Scene = Game GameScene
           | Setup SetupScene

makePrisms ''Scene

data AppState = AppState { _scene :: Scene
                         , _actionChan :: ActionChan
                         }

makeLenses ''AppState

data AppEvent = SetGameState GameState
              | StartNewGame GameState
              | Die String

type Name = ()

app :: App AppState AppEvent Name
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const theMap
          }

bgThread :: String -> Int -> BChan AppEvent -> ActionChan -> IO ()
bgThread host port chan actionChan = do
    man <- newManager defaultManagerSettings
    let url = BaseUrl Http host port ""
    let env = mkClientEnv man url

    forever $ do
        act <- readChan actionChan
        res <- case act of
          MakeMove s -> runClientM (makeMoveC s) env
          NewGame sz -> runClientM (newGameC sz) env
        let con = case act of
                    MakeMove _ -> SetGameState
                    _          -> StartNewGame
        case res of
          Left err -> writeBChan chan $ Die $ show err
          Right s' -> writeBChan chan $ con s'

clientMain :: String -> Int -> IO ()
clientMain host port = do
    chan <- newBChan 16
    actionChan <- newChan
    forkIO $ bgThread host port chan actionChan

    let initialState = AppState (Setup $ SetupScene Board3x3) actionChan

    let buildVty = V.mkVty V.defaultConfig
    initialVty <- buildVty
    void $ customMain initialVty buildVty (Just chan) app initialState

selectBoardSize :: Int -> BoardSize -> BoardSize
selectBoardSize 1    sz = if sz == maxBound then sz else succ sz
selectBoardSize (-1) sz = if sz == minBound then sz else pred sz
selectBoardSize 0    sz = sz

moveSel :: AppState -> Int -> Int -> AppState
moveSel s x y = s & scene._Game.sel %~ (movePos x y)
                  & scene._Setup.selSize %~ (selectBoardSize x)

gameScene :: GameState -> GameScene
gameScene s = GameScene s (getCellPos 0 0 (board s)) (curPlayer s)

handleEvent :: AppState -> BrickEvent Name AppEvent -> EventM Name (Next AppState)
handleEvent s (AppEvent (Die _))                    = halt s
handleEvent s (AppEvent (SetGameState gameState))   = continue $ s & scene._Game.game .~ gameState
handleEvent s (AppEvent (StartNewGame gameState))   = continue $ s & scene .~ (Game $ gameScene gameState)
handleEvent s (VtyEvent (V.EvKey V.KLeft []))       = continue $ moveSel s 0 (-1)
handleEvent s (VtyEvent (V.EvKey V.KRight []))      = continue $ moveSel s 0 1
handleEvent s (VtyEvent (V.EvKey V.KUp []))         = continue $ moveSel s (-1) 0
handleEvent s (VtyEvent (V.EvKey V.KDown []))       = continue $ moveSel s 1 0
handleEvent s (VtyEvent (V.EvKey V.KEnter []))      = do
    let sc = s ^. scene
    case sc ^? _Game of
      Just g -> do
          if gameOver $ board $ g ^. game
          then continue $ s & scene .~ (Setup $ SetupScene Board3x3)
          else if (g ^. me) == curPlayer (g ^. game) then do
              let s' = makeMove (g ^. sel) (g ^. game)
              liftIO $ writeChan (s ^. actionChan) $ MakeMove s'
              continue $ s & scene._Game.game .~ s'
          else continue s
      Nothing -> do
          liftIO $ writeChan (s ^. actionChan) $ NewGame (sc ^?! _Setup.selSize)
          continue s
handleEvent s (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt s
handleEvent s (VtyEvent (V.EvKey V.KEsc []))        = halt s
handleEvent s _                                     = continue s

drawUI :: AppState -> [Widget Name]
drawUI s =
    case (s ^. scene) of 
      Game sc -> [ C.center $ drawBoard sc ]
      Setup sc -> [ C.center $ drawSizeSelect sc ]

drawGameOver :: Maybe Player -> Widget Name
drawGameOver w =
    (C.hCenter $ str $ case w of
        Just p -> "Winner: " <> (show p)
        Nothing -> "Tie") <=>
    (C.hCenter $ str "\nPress ENTER to continue")

drawBoard :: GameScene -> Widget Name
drawBoard s
  | gameOver b = drawGameOver (winner b)
  | otherwise = withBorderStyle BS.unicodeBold
    $ B.borderWithLabel (str $ (show size) <> "x" <> (show size))
    $ vBox rows
    where rows       = [hBox $ cells r | r <- [0..size-1]]
          cells r    = [draw r c | c <- [0..size-1]]
          draw r c   = border r c $ padLeftRight 1 $ drawCell $ getCell (getCellPos r c b) b
          border r c = if showB r c then B.border else padAll 1
          b          = board $ s ^. game
          size       = boardSize b
          showB r c  = getCellPos r c b == s ^. sel

drawCell :: Cell -> Widget Name
drawCell (Just X) = withAttr xAttr $ str "X"
drawCell (Just O) = withAttr oAttr $ str "O"
drawCell Nothing  = withAttr emptyAttr $ str " "

drawSizeSelect :: SetupScene -> Widget Name
drawSizeSelect sc = vBox $ map (C.hCenter . str . decorate) [minBound..]
    where decorate sz = if sz == sc ^. selSize then ("> " <> show sz <> " <")
                                               else show sz

xAttr, oAttr, emptyAttr :: AttrName
xAttr = "xAttr"
oAttr = "oAttr"
emptyAttr = "emptyAttr"

theMap :: AttrMap
theMap = attrMap V.defAttr
    [ (xAttr, fg V.red `V.withStyle` V.bold)
    , (oAttr, fg V.blue `V.withStyle` V.bold)
    ]
