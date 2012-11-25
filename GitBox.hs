import System.IO
import Network

main :: IO ()
main = do server <- connectTo "localhost" $ PortNumber $ fromIntegral 9000
          line <- getLine 
          let f l = case l of 
                        "exit" -> hPutStr server $ send l
                        _      -> do hPutStr server $ send l
                                     l' <- getLine
                                     f l'
          f line  
          return ()


send l = l ++ "\n"