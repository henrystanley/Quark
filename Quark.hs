import QuarkInterpreter
import QuarkParser
import QuarkType
import System.Environment
import System.IO

-- entry point for the program
-- if quark is run with an argument it will be run as a script, otherwise it starts the REPL
main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  args <- getArgs
  case args of
    [] -> qRepl (return emptyQVM)
    (filename : _) -> do
      script <- readFile filename
      runQuark False (return emptyQVM) script >> return ()

-- read eval print loop
-- prints prompt, gets input, quits if input is '*q', evaulates the input, then prints the result (or an error)
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
