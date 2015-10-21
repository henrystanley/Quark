module QuarkType where

import qualified Data.Map as Map
import Data.List

--- Quark Types ---

data QItem = QNum Double -- Number
           | QQuote [QItem] [QItem] -- Quote
           | QAtom String -- Atom (function name or variable)
           | QSym String -- Symbol
           | QStr String -- String
           deriving (Eq, Show, Ord)

-- type used to store defined functions
type QLib = Map.Map String QItem

-- data stack type
type QStack = [QItem]

-- a tuple containing a data stack, a list of quark items to evaluate, and an index of functions
type QVM = ([QItem], [QItem], QLib)

-- getters for the QVM type

getStack :: QVM -> QStack
getStack (s, _, _) = s

getTokens:: QVM -> [QItem]
getTokens (_, t, _) = t

getLib :: QVM -> QLib
getLib (_, _, l) = l

-- converts a possibly nested quark item, into a string of the equivelent quark code
-- reverse parsing, if you will
serializeQ :: QItem -> String
serializeQ (QNum x) = show x
serializeQ (QAtom x) = x
serializeQ (QSym x) = ':' : x
serializeQ (QStr x) = "\"" ++ x ++ "\""
serializeQ (QQuote args vals) = "[ " ++ s_args ++ s_vals ++ " ]"
  where s_args = if args == [] then "" else intercalate " " (map serializeQ args) ++ " | "
        s_vals = intercalate " " $ map serializeQ vals


--- Quark Type Signatures ---

-- these are the types for the internal quark type system
-- kind of meta...
data QType = Num
           | Atom
           | Str
           | Sym
           | Quote QType QType
           | Empty -- empty quote patterns or bodies have this value
           | Any -- type of quote patterns or bodies with non-homogeneous types
           | NotEmpty -- a quark value will never have this type, it is only used in type checking
           deriving (Eq, Show)

-- a list of quark types, used in core function definitions
type QTypeSig = [QType]

-- converts a quark value into a quark type
-- in the case of quotes:
--   if the quote pattern or body is homogeneous, it will use this type
--   if the quote pattern or body is empty it will use the `Empty` type
--   otherwise it will use the `Any` type
-- thus: [ 1 2 3 | :cow 'fish' hund ] :: Quote Num Any
qtype :: QItem -> QType
qtype (QNum _) = Num
qtype (QAtom _) = Atom
qtype (QSym _) = Sym
qtype (QStr _) = Str
qtype (QQuote args items) = Quote (inner_type args) (inner_type items)
  where inner_type [] = Empty
        inner_type xs = foldl' consistent_type (qtype (head xs)) (map qtype xs)
        consistent_type (Quote x1 y1) (Quote x2 y2) = Quote (consistent_type x1 x2) (consistent_type y2 y2)
        consistent_type x y = if x == y then x else Any

-- converts the type of a quark item into a quark symbol
qtypeLiteral :: QType -> QItem
qtypeLiteral (Quote a b) = QQuote [] [(QSym "Quote"), qtypeLiteral a, qtypeLiteral b ]
qtypeLiteral x = QSym . show $ x

-- checks if two quark types match
-- oddball cases are:
--   Any, which matches everything
--   NotEmpty, which matches everything but `Empty`
--   and Quotes, which recursively compare their child types
qtypeCompare :: QType -> QType -> Bool
qtypeCompare Any _ = True
qtypeCompare NotEmpty Empty = False
qtypeCompare NotEmpty _ = True
qtypeCompare (Quote a1 b1) (Quote a2 b2) = (qtypeCompare a1 a2) && (qtypeCompare b1 b2)
qtypeCompare t1 t2 = t1 == t2

-- checks to see if the top of the stack matches a type signature
qtypeCheck :: QTypeSig -> QStack -> Bool
qtypeCheck sig stack = if length stack < (length sig) then False
  else foldl' (\z (x, y) -> z && (qtypeCompare x y)) True $ zip sig stack_sig
    where stack_sig = map qtype . reverse . take (length sig) $ stack
