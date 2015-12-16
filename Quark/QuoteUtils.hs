module Quark.QuoteUtils where

import Quark.Type
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Foldable (toList)


--- Quote Utilities ---

-- checks if a QItem is a QFunc
isFunc :: QItem -> Bool
isFunc (QFunc _) = True
isFunc _ = False

-- returns the AtomSet of a QProg (non recursive)
getAtoms :: QProg -> AtomSet
getAtoms = Set.fromList . map (\(QFunc a) -> a) . filter isFunc . toList

-- convert a QFunc to a QVar
funcToVar :: QItem -> QItem
funcToVar (QFunc x) = QVar x
funcToVar x = x

-- recursively replaces QFuncs with QVars
funcToVarRec :: AtomSet -> QItem -> QItem
funcToVarRec vars (QFunc x) = if Set.member x vars then QVar x else QFunc x
funcToVarRec vars (QQuote pattern body) = QQuote pattern' body'
  where vars' = Set.union vars $ getAtoms pattern
        pattern' = fmap funcToVar pattern
        body' = fmap (funcToVarRec vars') body
funcToVarRec _ x = x

-- same as funcToVarRec, but with default `vars` AtomSet
addVars :: QItem -> QItem
addVars = funcToVarRec Set.empty

-- returns a list of all QFuncs referenced in a QQuote
getFuncs :: QItem -> AtomSet
getFuncs (QQuote _ body) = Set.unions . toList $ fmap getFuncs body
getFuncs (QFunc a) = Set.singleton a
getFuncs _ = Set.empty

-- adds a prefix to a QVar
prefixVar :: FuncName -> QItem -> QItem
prefixVar prefix (QVar x) = QVar (prefix ++ "." ++ x)
prefixVar _ x =  x

-- recursively prefixes QVars
prefixVarRec :: FuncName -> QItem -> QItem
prefixVarRec prefix (QVar x) = QVar (prefix ++ "." ++ x)
prefixVarRec prefix (QQuote pattern body) = QQuote pattern' body'
  where pattern' = fmap (prefixVar prefix) pattern
        body' = fmap (prefixVarRec prefix) body
prefixVarRec _ x = x
