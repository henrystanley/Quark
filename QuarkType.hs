module QuarkType where

import qualified Data.Map as Map
import qualified Data.Sequence as Seq

--- Quark Types ---

data QItem = QNum Double -- Number
           | QQuote (Seq.Seq QItem) (Seq.Seq QItem) -- Quote
           | QAtom String -- Atom (function name or variable)
           | QSym String -- Symbol
           | QStr String -- String
           deriving (Eq, Show, Ord)

-- type used to store defined functions
type QLib = Map.Map String QItem

-- data stack type
type QStack = [QItem]

-- sequence to hold unevaluated items
type QProg = Seq.Seq QItem

-- a tuple containing a data stack, a list of quark items to evaluate, and an index of functions
type QVM = (QStack, QProg, QLib)

-- getters for the QVM type

getStack :: QVM -> QStack
getStack (s, _, _) = s

getTokens:: QVM -> QProg
getTokens (_, t, _) = t

getLib :: QVM -> QLib
getLib (_, _, l) = l

-- converts a possibly nested quark item, into a string of the equivelent quark code
-- reverse parsing, if you will
serializeQ :: QItem -> String
serializeQ (QNum x) = if (ceiling x) == (floor x) then (show . floor) x else show x
serializeQ (QAtom x) = x
serializeQ (QSym x) = ':' : x
serializeQ (QStr x) = "\"" ++ x ++ "\""
serializeQ (QQuote args vals) = "[" ++ s_args ++ s_vals ++ "]"
  where join_with_spaces sq = case (foldl (++) "" $ fmap ((" " ++) . serializeQ) sq) of
          [] -> ""
          xs -> xs ++ " "
        s_args = if Seq.null args then "" else join_with_spaces args ++ "|"
        s_vals = join_with_spaces vals


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
