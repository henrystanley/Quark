import QuarkInterpreter
import QuarkParser
import QuarkType
import System.Environment
import System.IO
import Paths_QuarkLang

-- entry point for the program
-- if quark is run with an argument it will be run as a script, otherwise it starts the REPL
main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  args <- getArgs
  let (filenames, params) = parseArgs args
  baseQVM <- if not $ elem "--only-core" params
    then do
      preludeFilepath <- getDataFileName "prelude.qrk"
      preludeCode <- readFile preludeFilepath
      preludeVM <- runQuark False (return emptyQVM) preludeCode
      case preludeVM of
        Just vm -> return vm
        Nothing -> return emptyQVM
    else (return emptyQVM)
  case filenames of
    [] -> qRepl $ return baseQVM
    xs -> mapM (qInterpret (return baseQVM)) filenames >> return ()


-- parses args into filenames and parameters
parseArgs :: [String] -> ([String], [String])
parseArgs = break (('-' ==) . head)

-- runs a quark script
qInterpret :: IO QVM -> String -> IO ()
qInterpret vm scriptname  = do
  script <- readFile scriptname
  runQuark False vm script
  return ()


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
