module Quark.Type where

import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set

--- Quark Types ---

data QItem = QNum Double -- Number
           | QQuote (Seq.Seq QItem) (Seq.Seq QItem) -- Quote
           | QFunc String -- Function
           | QVar String -- Variable
           | QSym String -- Symbol
           | QStr String -- String
           deriving (Eq, Show, Ord)

-- function names
type FuncName = String

-- type used to store defined functions
type QLib = Map.Map FuncName QItem

-- data stack type
type QStack = [QItem]

-- sequence to hold unevaluated items
type QProg = Seq.Seq QItem

-- set of function names
type AtomSet = Set.Set FuncName
