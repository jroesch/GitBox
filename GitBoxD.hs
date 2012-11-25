module GitBoxD where 

import System.IO
import System.FSNotify
import Network
import System.Environment (getEnvironment, getArgs)
import System.Exit
import Data.Text (pack)
import Filesystem.Path.CurrentOS (fromText)

main :: IO ()
main = do 
    argv <- getArgs 
    env <- getEnvironment
    gbpath <- "GITBOX_PATH" `lookup` env
    checkConfig gbpath 
    wm <- startManager
    forAllIn gbpath listE wm
    run 
    stopManager wm
    putStrLn "Done."

checkConfig :: Maybe String -> IO ()
checkConfig pt = case pt of 
  Nothing   -> putStrLn "Not configured."
  Just path -> return ()

-- run a daemon that does all git interation and commits, then make gitbox command line tool just talk to the daemon 
--pFromString :: String -> FilePath
pFromString s = fromText $ pack s

forAllIn :: String -> (Event -> IO ()) -> WatchManager -> IO ()
forAllIn path h = 
    \wm -> watchTree wm (pFromString path) (\_ -> True) h


listE :: Event -> IO ()
listE = \e -> case e of 
  Added    p time -> putStrLn p
  Modified p time -> putStrLn p
  Removed  p time -> putStrLn p

-- this networking code is nap as fuck, fix lates motherfucker
-- loop this shit 
run = do socket <- listenOn $ PortNumber $ fromIntegral 9000
         rloop socket 
         sClose socket
    where 
        req h = 
            do eof <- hIsEOF h
               case eof of 
                 True  -> return ()
                 False -> do line <- hGetLine h 
                             case line of
                                "exit" -> exitWith ExitSuccess
                                _      -> (print line) >> req h

        rloop sock = do (client, _, _) <- accept sock
                        req client
                        rloop sock
