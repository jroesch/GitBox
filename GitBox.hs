import System.IO
import Network
import Data.Map (Map)
import qualified Data.Map as M 

type Handler = Handle -> IO ()

main :: IO ()
main = route $ ["exit" `to` exitHandler]

mkRoutes :: [(String, Handler)] -> Map String Handler
mkRoutes = M.fromList

to :: String -> Handler -> (String, Handler)
to r h = (r, h)

route :: [(String, Handler)] -> IO ()
route rs = do
    server <- connectTo "localhost" $ PortNumber $ fromIntegral 9000
    line <- getLine
    routeF line server
  where routes = mkRoutes rs
        routeF k server = case (k `M.lookup` routes) of
                            Nothing -> putStrLn "error: No such command."
                            Just h -> h server

exitHandler server = hPutStrLn server "exit" 
