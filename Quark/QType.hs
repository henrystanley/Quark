module Quark.QType where

import Quark.Type

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
qtypeLiteral = QSym . show

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
