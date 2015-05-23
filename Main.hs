import Application () -- for YesodDispatch instance
import Import
import Yesod.Core

main :: IO ()
main = warp 4000 App
