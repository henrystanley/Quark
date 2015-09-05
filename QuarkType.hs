module QuarkType where

import qualified Data.Map as Map
import Data.List

-- Quark Types --

data QItem = QNum Double
           | QQuote [QItem] [QItem]
           | QAtom String
           | QSym String
           | QStr String
           deriving (Eq, Show, Ord)

type QLib = Map.Map String QItem

type QStack = [QItem]

type QVM = ([QItem], [QItem], QLib)

getStack :: QVM -> QStack
getStack (s, _, _) = s

getTokens:: QVM -> [QItem]
getTokens (_, t, _) = t

getLib :: QVM -> QLib
getLib (_, _, l) = l

serializeQ :: QItem -> String
serializeQ (QNum x) = show x 
serializeQ (QAtom x) = x 
serializeQ (QSym x) = ':' : x 
serializeQ (QStr x) = "\"" ++ x ++ "\""
serializeQ (QQuote args vals) = "[ " ++ s_args ++ s_vals ++ " ]"
  where s_args = if args == [] then "" else intercalate " " (map serializeQ args) ++ " | "
        s_vals = intercalate " " $ map serializeQ vals


-- Quark Type Signatures --

data QType = Num
           | Atom
           | Str
           | Sym
           | Quote QType QType
           | Empty
           | Any
           | NotEmpty
           deriving (Eq, Show)

type QTypeSig = [QType]

qtype :: QItem -> QType
qtype (QNum _) = Num
qtype (QAtom _) = Atom
qtype (QSym _) = Sym
qtype (QStr _) = Str
qtype (QQuote args items) = Quote (inner_type args) (inner_type items)
  where inner_type [] = Empty
        inner_type xs = foldl consistent_type (qtype (head xs)) (map qtype xs)
        consistent_type (Quote x1 y1) (Quote x2 y2) = Quote (consistent_type x1 x2) (consistent_type y2 y2)
        consistent_type x y = if x == y then x else Any
        
qtypeLiteral :: QType -> QItem
qtypeLiteral (Quote a b) = QQuote [] [(QSym "Quote"), qtypeLiteral a, qtypeLiteral b ]
qtypeLiteral x = QSym . show $ x

qtypeCompare :: QType -> QType -> Bool
qtypeCompare Any _ = True
qtypeCompare NotEmpty Empty = False
qtypeCompare NotEmpty _ = True
qtypeCompare (Quote a1 b1) (Quote a2 b2) = (qtypeCompare a1 a2) && (qtypeCompare b1 b2)
qtypeCompare t1 t2 = t1 == t2

qtypeCheck :: QTypeSig -> QStack -> Bool
qtypeCheck sig stack = if length stack < (length sig) then False
  else and . map (uncurry qtypeCompare) . zip sig $ stack_sig 
    where stack_sig = map qtype . reverse . take (length sig) $ stack
