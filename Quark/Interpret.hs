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
recEval :: [String] -> IO (Maybe QVM) -> IO (Maybe QVM)
recEval params iovm = do
  vm <- iovm
  case vm of
    Just (prog -> (viewl -> Seq.EmptyL)) -> return vm
    Just x -> recEval params $ eval params x
    Nothing -> return Nothing

-- parses and evaluates a string of quark code
runQuark :: [String] -> IO QVM -> String -> IO (Maybe QVM)
runQuark params iovm s = case qParse s of
  Left perror -> iovm >>= raiseError ("Parse Error: " ++ (show perror))
  Right tokens -> do
    vm <- iovm
    let vm' = (return . Just) (pushProgVM (Seq.fromList tokens) vm) in recEval params vm'

-- runs a quark script
qInterpret :: [String] -> IO QVM -> String -> IO ()
qInterpret params vm scriptname = do
  script <- readFile scriptname
  runQuark params vm script
  return ()
