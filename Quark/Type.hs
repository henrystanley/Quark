module Quark.Type where

import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import Data.Foldable (toList)

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


--- Quark State Type ---

-- a type containing a data stack, a list of quark items to evaluate, and an index of functions
data QVM = QVM { stack :: QStack
               , prog :: QProg
               , binds :: QLib
               } deriving (Show, Eq)

-- a base quark vm, obviously all quark programs start with this
emptyQVM :: QVM
emptyQVM = QVM [] Seq.empty Map.empty

-- concat items to a quark vm's token stack
pushProgQVM :: QVM -> QProg -> QVM
pushProgQVM vm newProg = vm { prog = newProg Seq.>< (prog vm) }

-- stack manipulation functions

dropVM :: Int -> QVM -> QVM
dropVM n vm = vm { stack = drop n (stack vm) }

pushVM :: QItem -> QVM -> QVM
pushVM x vm = vm { stack = x : (stack vm) }

-- interpreter state
type IState = IO (Maybe QVM)

-- a function that maps a QVM to an IO (Maybe QVM)
type QFunc = QVM -> IState


--- Serialization ---

-- converts a quark item into a string of the equivelent quark code
-- the number parameter `n` will shorten quotes to this length and add an ellipsis
-- if `n` is 0 no shortening will occur
serializeQ :: Int -> QItem -> String
serializeQ _ (QNum x) = if (ceiling x) == (floor x) then (show . floor) x else show x
serializeQ _ (QAtom x) = x
serializeQ _ (QSym x) = ':' : x
serializeQ _ (QStr x) = "\"" ++ x ++ "\""
serializeQ n (QQuote pattern body) = "[ " ++ patternStr ++ bodyStr ++ " ]"
  where getItems = if n == 0 then id else take n
        ellipsis xs = if (Seq.length xs > n) && (n /= 0) then " ..." else ""
        quoteSeqToStr = unwords . map (serializeQ n) . getItems . toList
        patternStr = if Seq.null pattern then "" else (quoteSeqToStr pattern) ++ (ellipsis pattern) ++ " | "
        bodyStr = (quoteSeqToStr body) ++ (ellipsis body)


--- Quark Type Signatures ---

-- these are the types for the internal quark type system
-- kind of meta...
data QType = Num
           | Atom
           | Str
           | Sym
           | Quote
           | Any -- wildcard
           deriving (Eq, Show)

-- a list of quark types, used in core function definitions
type QTypeSig = [QType]

-- converts a quark value into a quark type
qtype :: QItem -> QType
qtype (QNum _) = Num
qtype (QAtom _) = Atom
qtype (QSym _) = Sym
qtype (QStr _) = Str
qtype (QQuote _ _) = Quote

-- converts the type of a quark item into a quark symbol
qtypeLiteral :: QType -> QItem
qtypeLiteral x = QSym . show $ x

-- checks if two quark types match
-- an oddball case is `Any`, which matches everything
qtypeCompare :: QType -> QType -> Bool
qtypeCompare Any _ = True
qtypeCompare t1 t2 = t1 == t2

-- checks to see if the top of the stack matches a type signature
qtypeCheck :: QTypeSig -> QStack -> Bool
qtypeCheck [] _ = True
qtypeCheck (x:xs) [] = False
qtypeCheck (x:xs) (y:ys) = if qtypeCompare x (qtype y)
  then qtypeCheck xs ys
  else False
