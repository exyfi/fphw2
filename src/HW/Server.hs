module HW.Server where

import HW.Game
import Data.Proxy
import Data.Aeson
import Servant.API
import Servant.Server
import Servant.Client
import Network.Wai.Handler.Warp

type GameAPI = "move" :> ReqBody '[JSON] GameState :> Post '[JSON] GameState
          :<|> "game" :> ReqBody '[JSON] BoardSize :> Post '[JSON] GameState

serverMain :: IO ()
serverMain = run 31415 $ serve (Proxy @GameAPI) (makeMoveHandler :<|> newGameHandler)

makeMoveHandler :: GameState -> Handler GameState
makeMoveHandler = return . makeBestMove

newGameHandler :: BoardSize -> Handler GameState
newGameHandler = return . newGame

makeMoveC :<|> newGameC = client (Proxy @GameAPI)
