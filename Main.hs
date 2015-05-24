import Application () -- for YesodDispatch instance
import Data.Maybe
import Control.Applicative
import Import
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors
import Yesod.Core
import Graph
import Types
import qualified Data.IntMap as IM
import System.Environment

loadApp :: IO App
loadApp = do
    paths <- fromMaybe (error "error loading graph") <$>
        parseJSONFromFile "paths.json"
    let graph = buildGraph paths
    return $ App graph (stationsToMap paths)

stationsToMap = IM.fromList . map spToPair
  where spToPair sp@(StationPaths (Station number _ _) _) = (number, sp)

main :: IO ()
main = do
    port <- maybe 4000 read <$> lookupEnv "PORT"
    app <- loadApp
    waiApp <- toWaiApp app
    run port $ simpleCors waiApp
