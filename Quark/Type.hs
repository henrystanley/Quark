module Quark.Type where

import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set

--- Quark Types ---

data QItem = QNum Double -- Number
           | QQuote (Seq.Seq QItem) (Seq.Seq QItem) -- Quote
           | QFunc String -- Function
           | QCFunc String -- Core Function
           | QVar String -- Variable
           | QSym String -- Symbol
           | QStr String -- String
           | QMagic IMagic -- Interpreter helper value
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

-- hidden interpreter commands
-- used to handle things like managing the callstack or marking optimizations
data IMagic = PopCallStack deriving (Eq, Show, Ord)
