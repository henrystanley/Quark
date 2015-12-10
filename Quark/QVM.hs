module Quark.QVM where

import Quark.Type
import qualified Data.Sequence as Seq
import qualified Data.Map as Map

--- Quark State Type ---

-- a type containing a data stack, a list of quark items to evaluate, and an index of functions (with their inlined versions)
data QVM = QVM { stack :: QStack
               , prog :: QProg
               , binds :: QLib
               , i_binds :: Map.Map FuncName (Maybe QItem)
               } deriving (Show, Eq)

-- interpreter state
type IState = IO (Maybe QVM)

-- a function that maps a QVM to an IO (Maybe QVM)
-- all quark core functions have this type
type QFunc = QVM -> IState

-- a base quark vm, obviously all quark programs start with this
emptyQVM :: QVM
emptyQVM = QVM [] Seq.empty Map.empty Map.empty

-- concat items to a quark vm's evaluation stack
pushProgQVM :: QVM -> QProg -> QVM
pushProgQVM vm newProg = vm { prog = newProg Seq.>< (prog vm) }

-- drop n items from a vm's stack
dropVM :: Int -> QVM -> QVM
dropVM n vm = vm { stack = drop n (stack vm) }

-- push a qitem to a vm's stack
pushVM :: QItem -> QVM -> QVM
pushVM x vm = vm { stack = x : (stack vm) }

-- gets a runtime defined function, if no such function exists returns `Nothing`
getDef :: QVM -> FuncName -> Maybe QItem
getDef vm fname = Map.lookup fname (binds vm)

-- gets a runtime defined function in its inlined form, if no such function exists returns `Nothing`
getIDef :: QVM -> FuncName -> Maybe QItem
getIDef vm fname = (Map.lookup fname (i_binds vm)) >>= id
