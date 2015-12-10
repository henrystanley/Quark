module Quark.Inline where

import Quark.Type
import Quark.QVM
import Quark.QuoteEval
import Data.Maybe
import Data.Sequence (viewr)
import Data.Sequence (ViewR(..))
import Data.Foldable (toList)
import qualified Data.Sequence as Seq
import qualified Data.Map.Strict as Map
import qualified Data.List as List
import qualified Data.Set as Set


--- Atom Sets ---

type AtomSet = Set.Set FuncName

-- checks if a QItem is a QAtom
isAtom :: QItem -> Bool
isAtom (QAtom _) = True
isAtom _ = False

-- returns the AtomSet of a QProg (non recursive)
getAtoms :: QProg -> AtomSet
getAtoms = Set.fromList . map (\(QAtom a) -> a) . filter isAtom . toList

-- returns an AtomSet of atoms from QQuote bodies (recursively searches QQuotes)
getBodyAtoms :: QItem -> AtomSet
getBodyAtoms (QQuote _ body) = Set.unions $ fmap getBodyAtoms body
getBodyAtoms (QAtom a) = Set.singleton a
getBodyAtoms _ = Set.empty

-- returns an AtomSet of vars in a QItem (recursively searches QQuotes)
getVars :: QItem -> AtomSet
getVars (QQuote pattern body) = Set.union (getAtoms pattern) (Set.unions $ fmap getVars body)
getVars _ = Set.empty

-- returns an AtomSet of functions in a QItem (recursively searches QQuotes)
getFuncs :: QItem -> AtomSet
getFuncs item = Set.difference (getBodyAtoms item) (getVars item)

-- returns a set of function definitions for all defined members of an AtomsSet
getDefs :: QVM -> AtomSet -> Set.Set QItem
getDefs vm = Set.map (\(Just x) -> x) . Set.filter isJust . Set.map (getDef vm)

--- AtomSet Filters ---

-- AtomSet of core functions (functions implemented in Haskell instead of Quark)
coreFuncs :: AtomSet
coreFuncs = Set.fromList [ "+", "*", "/", "<", "<<", ">>", "@+", "@-"
                         , "show", "chars", "weld", "type", "def"
                         , "parse", "call", "match", ".", "load"
                         , "write", "cmd", "print", "exit" ]

-- checks if a function is a core function
isCoreFunc :: FuncName -> Bool
isCoreFunc func = Set.member func coreFuncs

-- filters core functions out of an AtomSet
nonCoreFuncs :: AtomSet -> AtomSet
nonCoreFuncs = Set.filter (not . isCoreFunc)


-- checks if a function is recursive or co-recursive
isRecursive :: QVM -> FuncName -> Bool
isRecursive vm func = isRecursive' (Set.singleton func)
  where isRecursive' funcs = if Set.null $ Set.intersect funcDeps funcs
          then and $ map (\fDep -> isRecursive' $ Set.insert fDep funcs) funcDeps
          else True
          where funcDeps = nonCoreFuncs . Set.unions . getDefs $ funcs

-- filters recursive functions out of an AtomSet
nonRecursive :: QVM -> AtomSet -> AtomSet
nonRecursive vm = Set.filter (not . isRecursive vm)


-- checks if a function is defined
isDefined :: QVM -> FuncName -> Bool
isDefined vm = isJust . getDef vm

-- filters undefined functions out of an AtomSet
onlyDefined :: AtomSet -> AtomSet
onlyDefined vm = Set.filter (isDefined vm)


-- returns an AtomSet of functions from a QItem that can be safely inlined
inlinableFuncs :: QVM -> QItem -> AtomSet
inlinableFuncs vm = nonRecursive vm . onlyDefined vm . nonCoreFuncs . getFuncs


--- Inlining ---


-- recursively calculates which functions will be need to be re-inlined if a function is updated
changedDefs :: QVM -> FuncName -> AtomSet
changedDefs vm func = changedDefs' $ Set.singleton func
  where changedDefs' funcs = if funcs == funcs' then funcs else changedDefs' $ Set.union funcs funcs'
          where funcs' =


  if funcs == funcs' then funcs else changedDefs vm funcs' ++ funcs
  where funcs' = Map.keys $ Map.filter (not . null . List.intersect funcs . getFuncs []) (binds vm)

-- computes the inlined version of a function and recurses over any sub-functions of the function
updateInline :: FuncName -> QVM -> QVM
updateInline fname vm = if isJust i_f
  then vm
  else inlineFunc fname to_inline_in_f $ foldr updateInline vm to_inline_in_f
  where i_f = (i_binds vm) Map.! fname
        f = (binds vm) Map.! fname
        to_inline_in_f = inlinableFuncs f $ binds vm

-- inlines a function
inline :: QItem -> QVM -> QItem
inline item vm = if null inlinable then item else inlineSub item vm inlinable
  where inlinable = inlinableFuncs item $ binds vm


inlineSub :: QItem -> QVM -> [FuncName] -> QItem
inlineSub item vm to_sub =
  where

-- sets a function's inline slot to its inlined form
inlineFunc :: FuncName -> [FuncName] -> QVM -> QVM
inlineFunc fname to_inline vm = vm { i_binds = Map.insert fname (Just f_inlined) (i_binds vm) }
  where f_inlined = head $ inline ((binds vm) Map.! fname) to_inline vm

hygenicVars :: QItem -> Int -> QItem
hygenicVars (QQuote p b) depth = qsub vars_delta (QQuote p b)
  where vars = getAtoms p
        vars' = map (\x -> QAtom $ x ++ (show depth)) vars
        vars_delta = Map.fromList $ zip vars vars'
hygenicVars x _ = x

-- Note: No var renaming yet... (this will cause problems)
inline :: QItem -> Int -> QVM -> QItem
inline (QQuote pattern body) depth vm =
  where to_inline = inlinableFuncs (QQuote pattern body) $ binds vm
        (QQuote pattern' body') = hygenicVars (QQuote pattern body) depth
        inline_sub (QQuote =

inline (QAtom a) depth vm =
inline x _ _ = x


  where
        sub_atom (QAtom a) = if elem a to_inline then [inline_expr a vars vm, QAtom "call"]
        sub_atom x = [x]

        sub_quote vars' (QQuote pattern body) = [QQuote pattern $ Seq.fromList . concat $ inlined_body]
          where inlined_body = fmap (\x -> inline x to_inline vars' vm) body


inline (QQuote pattern body) to_inline vars vm = [QQuote pattern $ Seq.fromList . concat $ inlined_body]
  where inlined_body = fmap (\x -> inline x to_inline vm) body
inline (QAtom a) to_inline vars vm = if elem a to_inline
  then let inline_expr = (\(Just x) -> x) $ (i_binds vm) Map.! a in [inline_expr, QAtom "call"]
  else [QAtom a]
inline x _ _ _ = [x]
