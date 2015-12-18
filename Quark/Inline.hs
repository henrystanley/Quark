module Quark.Inline where

import Quark.Type
import Quark.QVM
import Quark.QuoteUtils
import Quark.Optimize
import Data.Maybe
import Data.Sequence (viewr)
import Data.Sequence (ViewR(..))
import qualified Data.Sequence as Seq
import qualified Data.Map.Strict as Map
import qualified Data.List as List
import qualified Data.Set as Set
import Data.Foldable (toList)


--- AtomSet Filters ---

-- returns a set of function definitions for all defined members of an AtomsSet
getDefs :: QVM -> AtomSet -> Set.Set QItem
getDefs vm = Set.map (\(Just x) -> x) . Set.filter isJust . Set.map (getDef vm)

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
  where isRecursive' funcs = if Set.null $ Set.intersection funcDeps funcs
          then or $ Set.map (\fDep -> isRecursive' $ Set.insert fDep funcs) funcDeps
          else True
          where funcDeps = nonCoreFuncs . Set.unions . Set.toList . Set.map getFuncs . getDefs vm $ funcs

-- filters recursive functions out of an AtomSet
nonRecursive :: QVM -> AtomSet -> AtomSet
nonRecursive vm = Set.filter (not . isRecursive vm)

-- checks if a function is defined
isDefined :: QVM -> FuncName -> Bool
isDefined vm = isJust . getDef vm

-- filters undefined functions out of an AtomSet
onlyDefined :: QVM -> AtomSet -> AtomSet
onlyDefined vm = Set.filter (isDefined vm)


-- returns an AtomSet of functions from a QItem that can be safely inlined
inlinableFuncs :: QVM -> QItem -> AtomSet
inlinableFuncs vm = nonRecursive vm . onlyDefined vm . nonCoreFuncs . getFuncs


--- Inlining ---

-- recursively calculates which functions will be need to be re-inlined if a function is updated
changedDefs :: QVM -> FuncName -> AtomSet
changedDefs vm func = changedDefs' $ Set.singleton func
  where changedDefs' funcs = if funcs == funcs' then funcs' else changedDefs' funcs'
          where funcs' = Set.union funcs $ Set.fromList . Map.keys $ dependent_funcs
                dependent_funcs = Map.filter (not . Set.null . Set.intersection funcs . getFuncs) (binds vm)

-- sets i_bind val to Nothing for inlined functions that need updating
markForInlineUpdate :: QVM -> FuncName -> QVM
markForInlineUpdate vm func = vm { i_binds = Set.foldr insert_nothing (i_binds vm) $ changedDefs vm func }
  where insert_nothing f m = Map.insert f Nothing m

-- updates an inlined function and recurses over any sub-functions
updateInline :: FuncName -> QVM -> QVM
updateInline func vm = if isJust $ (i_binds vm) Map.! func
  then vm
  else inlineFunc func $ Set.foldr updateInline vm func_deps
  where func_deps = inlinableFuncs vm $ (binds vm) Map.! func

-- builds and binds the inlined version of a function
inlineFunc :: FuncName -> QVM -> QVM
inlineFunc func vm = vm { i_binds = Map.insert func (Just func_def') (i_binds vm) }
  where func_def = (binds vm) Map.! func
        func_def' = inline func_def vm

-- inlines and optimizes a Quark item
inline :: QItem -> QVM -> QItem
inline item vm = optimize item'
  where item_deps = onlyDefined vm . nonCoreFuncs . getFuncs $ item
        item_dep_defs = getHygenicBinds item_deps vm
        item' = inlineSub item item_dep_defs

-- builds a map of function names to inlined and hygenically renamed function definitions
getHygenicBinds :: AtomSet -> QVM -> QLib
getHygenicBinds funcs vm = Map.fromList . toList $ Set.map (\f -> (f, hygenicBind f)) funcs
  where hygenicBind func = prefixVarRec func func_def
          where func_def = case (i_binds vm) Map.! func of { Just x -> x; _ -> (binds vm) Map.! func }

-- recursively replaces functions with their inlined values
inlineSub :: QItem -> QLib -> QItem
inlineSub item inlines = head $ inlineSub' item
  where inlineSub' (QFunc a) = case Map.lookup a inlines of
          Just x -> [x, QFunc "call"]
          Nothing -> [QFunc a]
        inlineSub' (QQuote p b) = [QQuote p $ Seq.fromList . concat . fmap inlineSub' $ b]
        inlineSub' x = [x]
