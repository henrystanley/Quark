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
eval :: QVM -> IState
eval vm = case viewl (prog vm) of
  ((QFunc f) :< sq) -> (getFunc vm f) $ vm { prog = sq }
  (x :< sq) -> return . Just $ vm { stack = x : (stack vm), prog = sq }

-- tries to retrieve a core or runtime defined function
-- if this fails, returns a `No such function: ` yeilding function
getFunc :: QVM -> FuncName -> QFunc
getFunc vm fname = case Map.lookup fname coreFunc of
  Just f -> f
  Nothing -> case getIDef vm fname of
    Just f -> tryQuote f
    Nothing -> (\_ -> raiseError ("No such function: " ++ fname))
