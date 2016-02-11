{-# LANGUAGE ViewPatterns #-}

module Quark.Eval (eval) where

import Quark.Type
import Quark.QVM
import Quark.QType
import Quark.Serialize
import Quark.Parse
import Quark.QuoteEval
import Quark.CoreFunc
import Quark.CoreFuncUtils
import Quark.Errors
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import Data.Sequence ((><), (<|), (|>), viewr, viewl)
import Data.Sequence (ViewL(..))
import Data.Sequence (ViewR(..))
import Data.Maybe


--- Evaluation ---

-- main evaluation function, takes a quark vm and (assuming there aren't any errors) returns its reduction
-- if the top item is a fuction, run it
-- otherwise, push the top item to the data stack
eval :: [String] -> QVM -> IState
eval params vm = case viewl (prog vm) of
  ((QFunc f) :< sq) -> (getFunc params vm f) $ vm { prog = sq }
  ((QMagic PopCallStack) :< sq) -> return . Just $ vm { prog = sq, callstack = tail $ callstack vm }
  (x :< sq) -> return . Just $ vm { stack = x : (stack vm), prog = sq }

-- tries to retrieve a core or runtime defined function
-- if this fails, returns a `No such function: ` yeilding function
getFunc :: [String] -> QVM -> FuncName -> QFunc
getFunc params vm fname = case Map.lookup fname (coreFunc params) of
  Just f -> f
  Nothing -> case runtimeDef of
    Just f -> callFunc fname f
    Nothing -> (\_ -> raiseError ("No such function: " ++ fname) vm)
  where runtimeDef = if elem "-O" params then getIDef vm fname else getDef vm fname
