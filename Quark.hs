{-# LANGUAGE ViewPatterns #-}

import Quark.Eval
import Quark.Parse
import Quark.Type
import Quark.QVM
import Quark.QType
import Quark.Errors
import Quark.Serialize
import Quark.REPL
import Quark.Interpret
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
