module Quark.Type where

import qualified Data.Map as Map
import qualified Data.Sequence as Seq

--- Quark Types ---

data QItem = QNum Double -- Number
           | QQuote (Seq.Seq QItem) (Seq.Seq QItem) -- Quote
           | QAtom String -- Atom (function name or variable)
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