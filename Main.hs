import Application () -- for YesodDispatch instance
import Data.Maybe
import Control.Applicative
import Import
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors
import Yesod.Core
import Graph

loadApp :: IO App
loadApp = do
    graph <- fromMaybe (error "error loading graph") <$>
        readGraphFromFile "paths.json"
    return $ App graph

main :: IO ()
main = do
    app <- loadApp
    waiApp <- toWaiApp app
    run 4000 $ simpleCors waiApp
