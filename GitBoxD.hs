{-# LANGUAGE OverloadedStrings #-}
module GitBoxD where 

import System.IO
import Data.IORef
import System.FSNotify
import Network
import System.Environment (getEnvironment, getArgs)
import System.Exit
import qualified Data.Text as T
import Filesystem.Path.CurrentOS (toText, fromText)
import Data.Time.Clock (UTCTime(..), DiffTime(..), secondsToDiffTime)
import Data.Time.Calendar (Day(..))
import Text.Printf
import Control.Monad
import System.Cmd

main :: IO ()
main = do 
    argv <- getArgs 
    env <- getEnvironment
    case "GITBOX_PATH" `lookup` env of
      Nothing   -> error "No GITBOX_PATH found."
      Just path -> do 
        wm <- startManager
        system "cd " ++ path
        (path `eachFile` trackInGit) wm
        run 
        stopManager wm
        putStrLn "Shutting down..."

pathFromString s = fromText $ T.pack s

eachFile :: String -> (Event -> IO ()) -> WatchManager -> IO ()
eachFile path h wm = watchTree wm (pathFromString path) (\_ -> True) h

lastTimeStamp :: IO (IORef UTCTime)
lastTimeStamp = newIORef (UTCTime (ModifiedJulianDay 0) (secondsToDiffTime 0))
  
run :: IO ()
run = do 
  socket <- listenOn $ PortNumber 9000
  (clientH, _, _) <- accept socket
  handleRequests clientH
  sClose socket

handleRequests :: Handle -> IO ()
handleRequests  h = do 
  eof <- hIsEOF h
  case eof of
    True  -> return ()
    False -> do
      line <- hGetLine h
      case line of
        "exit" -> exitSuccess
        _      -> (print line) >> handleRequests h

addMsg :: String
addMsg = "add %s at %s"

modMsg :: String
modMsg = "update %s at %s"

rmvMsg :: String
rmvMsg = "remove %s at %s"

commitCmd :: String
commitCmd = "cgit add %s; git commit -am \"%s\""

-- split this into many function, need to ignore .git directory
-- and make sure I set the working directory correctly
trackInGit e = do 
    let (msg, path, time) = case e of
                              Added p time    -> (addMsg, p, time)
                              Modified p time -> (modMsg, p, time)
                              Removed p time  -> (rmvMsg, p, time)
    let rawPath = (case toText path of
                    Right p -> T.unpack p
                    Left p  -> T.unpack p) :: String
        commitMsg = (printf msg rawPath $ show time) :: String
    lastCommitRef <- lastTimeStamp
    lastCommit <- readIORef lastCommitRef
    when (lastCommit < time) $ do 
      writeIORef lastCommitRef time
      let cmd = printf commitCmd rawPath commitMsg
      system cmd
      return ()
    


