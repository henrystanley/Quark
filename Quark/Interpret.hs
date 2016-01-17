{-# LANGUAGE ViewPatterns #-}

module Quark.Interpret (runQuark, qInterpret) where

import Quark.QVM
import Quark.Eval
import Quark.Parse
import Quark.Errors
import Data.Sequence (viewl)
import Data.Sequence (ViewL(..))
import qualified Data.Sequence as Seq

--- Interpreter ---

-- reduces a quark vm until its token stack is empty
recEval :: IO (Maybe QVM) -> IO (Maybe QVM)
recEval iovm = do
  vm <- iovm
  case vm of
    Just (prog -> (viewl -> Seq.EmptyL)) -> return vm
    Just x -> recEval $ eval x
    Nothing -> return Nothing

-- parses and evaluates a string of quark code
runQuark :: Bool -> IO QVM -> String -> IO (Maybe QVM)
runQuark quiet iovm s = case qParse s of
  Left perror -> if not quiet then raiseError $ "Parse Error: " ++ (show perror) else return Nothing
  Right tokens -> do
    vm <- iovm
    let vm' = (return . Just) (pushProgQVM vm (Seq.fromList tokens)) in recEval vm'

-- runs a quark script
qInterpret :: IO QVM -> String -> IO ()
qInterpret vm scriptname  = do
  script <- readFile scriptname
  runQuark False vm script
  return ()
