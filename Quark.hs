import QuarkInterpreter
import QuarkParser
import QuarkType
import System.Environment
import System.IO
import Paths_QuarkLang
import qualified Data.Map as Map
import Data.List

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


-- read eval loop
-- prints prompt, gets input, then handles special cmds or evaulates the input
qRepl :: IO QVM -> IO ()
qRepl vm = do
  putStr ":> "
  input <- getLine
  case words input of
    ["*q"] -> return ()
    ["*f"] -> vm >>= displayFunctions >> qRepl vm
    ["*f", f] -> vm >>= (displayFunction f) >> qRepl vm
    otherwise -> do
      reduced <- runQuark False vm input
      case reduced of
        Nothing -> qRepl vm
        Just vm' -> do
          displayStack vm'
          qRepl (return vm')

-- prints the stack
displayStack :: QVM -> IO ()
displayStack vm = putStrLn stackStr
  where stackStr = intercalate " " . map safeSerializeQ . reverse . getStack $ vm


-- print all runtime defined functions from a QVM
displayFunctions :: QVM -> IO ()
displayFunctions (_, _, funcs) = mapM putStr toPrint >> return ()
  where toPrint = map (\(f, v) -> f ++ "\n    " ++ (serializeQ v) ++ "\n\n") $ Map.assocs funcs

-- print a specific runtime defined function from a QVM
displayFunction :: String -> QVM -> IO ()
displayFunction f (_, _, funcs) = putStrLn $ case Map.lookup f funcs of
  Just func -> serializeQ func
  Nothing -> "No such function: " ++ f
