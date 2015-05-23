import Application () -- for YesodDispatch instance
import Import
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors
import Yesod.Core

main :: IO ()
main = do
     waiApp <- toWaiApp App
     run 4000 $ simpleCors waiApp
