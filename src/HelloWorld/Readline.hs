module HelloWorld.Readline
    (
      readline
    , loadHistory
    ) where

import qualified System.Console.Readline as RL

import Control.Monad (when)
import System.Directory (getHomeDirectory, doesFileExist)

import System.IO (hGetLine, hFlush, hIsEOF, stdin, stdout)
import System.IO.Error (tryIOError)

historyFile :: IO [Char]
historyFile = do
    home <- getHomeDirectory
    return $ home ++ "/.hello-world-history"

loadHistory :: IO ()
loadHistory = do
    hfile <- historyFile
    fileExists <- doesFileExist hfile
    when fileExists $ do
        content <- readFile hfile
        mapM RL.addHistory (lines content)
        return ()
    return ()

readline :: String -> IO (Maybe String)
readline prompt = do
    hfile <- historyFile
    maybeLine <- RL.readline prompt
    case maybeLine of
         Just "" -> return maybeLine
         Just line -> do
             RL.addHistory line
             res <- tryIOError (appendFile hfile (line ++ "\n"))
             return maybeLine
         _ -> return maybeLine
