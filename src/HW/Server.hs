module HW.Server where

import HW.Game
import Data.Proxy
import Data.Aeson
import Servant.API
import Servant.Server
import Servant.Client
import Network.Wai.Handler.Warp
import System.Random
import Control.Monad.IO.Class

type GameAPI = "move" :> ReqBody '[JSON] GameState :> Post '[JSON] GameState
          :<|> "game" :> ReqBody '[JSON] BoardSize :> Post '[JSON] GameState

serverMain :: Int -> IO ()
serverMain port = run port $ serve (Proxy @GameAPI) (makeMoveHandler :<|> newGameHandler)

makeMoveHandler :: GameState -> Handler GameState
makeMoveHandler = return . makeBestMove

newGameHandler :: BoardSize -> Handler GameState
newGameHandler sz = do
    let b = newGame sz
    x <- liftIO randomIO
    return $ if x then b else makeBestMove b

makeMoveC :<|> newGameC = client (Proxy @GameAPI)
