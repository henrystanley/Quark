module Quark.QVM where

import Quark.Type
import qualified Data.Sequence as Seq
import qualified Data.Map as Map

--- Quark State Type ---

-- a type containing a data stack, a list of quark items to evaluate, and an index of functions
data QVM = QVM { stack :: QStack
               , prog :: QProg
               , binds :: QLib
               } deriving (Show, Eq)

-- interpreter state
type IState = IO (Maybe QVM)

-- a function that maps a QVM to an IO (Maybe QVM)
type QFunc = QVM -> IState

-- a base quark vm, obviously all quark programs start with this
emptyQVM :: QVM
emptyQVM = QVM [] Seq.empty Map.empty

-- concat items to a quark vm's evaluation stack
pushProgQVM :: QVM -> QProg -> QVM
pushProgQVM vm newProg = vm { prog = newProg Seq.>< (prog vm) }

-- drop n items from a vm's stack
dropVM :: Int -> QVM -> QVM
dropVM n vm = vm { stack = drop n (stack vm) }

-- push a qitem to a vm's stack
pushVM :: QItem -> QVM -> QVM
pushVM x vm = vm { stack = x : (stack vm) }
