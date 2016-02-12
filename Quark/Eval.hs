module Quark.Eval (eval) where

import Quark.Type
import Quark.QVM
import Quark.QType
import Quark.Serialize
import Quark.Parse
import Quark.QuoteEval
import Quark.CoreFunc
import Quark.Errors
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import Data.Sequence ((><), (<|), (|>), viewr, viewl)
import Data.Sequence (ViewL(..))
import Data.Sequence (ViewR(..))
import Data.Maybe


--- Evaluation ---

-- main evaluation function
-- takes a quark vm and (assuming there aren't any errors) returns its reduction
--   if the top item is a runtime or core fuction, run it
--   if the top item is a magic interpreter command, execute it
--   otherwise, push the top item to the data stack
eval :: [String] -> QVM -> IState
eval params vm = case viewl (prog vm) of
  ((QFunc f) :< sq) -> libFunc f $ vm { prog = sq }
  ((QCFunc cf) :< sq) -> coreFunc cf $ vm { prog = sq }
  ((QMagic PopCallStack) :< sq) -> return . Just $ vm { prog = sq, callstack = tail $ callstack vm }
  (x :< sq) -> return . Just $ vm { stack = x : (stack vm), prog = sq }

-- tries to call a runtime defined function
libFunc :: FuncName -> QVM -> IState
libFunc fname vm = case getDef vm fname of
  Just f -> callFunc fname f vm
  Nothing -> raiseError ("No such function: " ++ fname) vm
