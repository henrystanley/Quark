import QuarkInterpreter
import QuarkParser
import QuarkType
import System.Environment
import System.IO

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  args <- getArgs
  case args of 
    [] -> qRepl (return emptyQVM)
    (filename : _) -> do
      script <- readFile filename
      runQuark False (return emptyQVM) script >> return () 

qRepl :: IO QVM -> IO ()
qRepl vm = do
  putStr ":> "
  input <- getLine
  case input of
    "*q" -> return ()
    str -> do
      reduced <- runQuark False vm str 
      case reduced of
        Nothing -> qRepl vm
        Just vm' -> qRepl (return vm')
        

