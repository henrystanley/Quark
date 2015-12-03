{-# LANGUAGE ViewPatterns #-}

import Quark.Eval
import Quark.Parse
import Quark.Type
import Quark.QVM
import Quark.QType
import Quark.Errors
import Quark.Serialize
import System.Environment
import System.IO
import Paths_QuarkLang
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import Data.Sequence (viewr, viewl)
import Data.Sequence (ViewL(..))
import Data.Sequence (ViewR(..))
import Data.List

--- Main ---

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
  where stackStr = intercalate " " . map (serializeQ 20) . reverse . stack $ vm

-- print all runtime defined functions from a QVM
displayFunctions :: QVM -> IO ()
displayFunctions vm = mapM putStr toPrint >> return ()
  where toPrint = map (\(f, v) -> f ++ "\n    " ++ (serializeQ 0 v) ++ "\n\n") $ Map.assocs (binds vm)

-- print a specific runtime defined function from a QVM
displayFunction :: String -> QVM -> IO ()
displayFunction fname vm = putStrLn $ case Map.lookup fname (binds vm) of
  Just func -> serializeQ 0 func
  Nothing -> "No such function: " ++ fname


--- Interpreter ---

-- parses and evaluates a string of quark code
runQuark :: Bool -> IO QVM -> String -> IO (Maybe QVM)
runQuark quiet iovm s = case qParse s of
  Left perror -> if not quiet then raiseError $ "Parse Error: " ++ (show perror) else return Nothing
  Right tokens -> do
    vm <- iovm
    let vm' = (return . Just) (pushProgQVM vm (Seq.fromList tokens)) in recEval vm'

-- reduces a quark vm until its token stack is empty
recEval :: IO (Maybe QVM) -> IO (Maybe QVM)
recEval iovm = do
  vm <- iovm
  case vm of
    Just (QVM _ (viewl -> Seq.EmptyL) _) -> return vm
    Just x -> recEval $ eval x
    Nothing -> return Nothing
