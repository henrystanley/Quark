module Quark.Inline where

import Quark.Type
import Quark.QVM
import Data.Maybe
import Data.Sequence (viewr)
import Data.Sequence (ViewR(..))
import Data.Foldable (toList)
import qualified Data.Sequence as Seq
import qualified Data.Map.Strict as Map
import qualified Data.List as List


--- Inlining Quotes ---


builtIns :: [FuncName]
builtIns = [ "+", "*", "/", "<", "<<", ">>", "@+", "@-"
           , "show", "chars", "weld", "type", "def"
           , "parse", "call", "match", ".", "load"
           , "write", "cmd", "print", "exit" ]

isAtom :: QItem -> Bool
isAtom (QAtom _) = True
isAtom _ = False

getAtoms :: QProg -> [FuncName]
getAtoms = map (\(QAtom a) -> a) . filter isAtom . toList

getFuncs :: [FuncName] -> QItem -> [FuncName]
getFuncs vars (QQuote pattern body) = concat $ fmap (getFuncs vars') body
  where vars' = (getAtoms pattern) ++ vars
getFuncs vars (QAtom a) = if (elem a vars) || (elem a builtIns) then [] else [a]
getFuncs _ _ = []

isRecursive :: QItem -> [FuncName] -> QLib -> Bool
isRecursive q uppers binds = if null funcs then False else sub_rec
  where funcs = getFuncs [] q
        sub_rec = if null $ List.intersect uppers funcs
          then and $ map (\fname -> isRecursive (binds Map.! fname) (fname:uppers) binds) funcs
          else True

inlinableFuncs :: QItem -> QLib -> [FuncName]
inlinableFuncs q binds = nonRecursive . defined . getFuncs [] $ q
  where defined = filter (\f -> not . isNothing $ Map.lookup f binds)
        nonRecursive = filter (\f -> not $ isRecursive (binds Map.! f) [f] binds)

changedDefs :: QVM -> [FuncName] -> [FuncName]
changedDefs vm funcs = if funcs == funcs' then funcs else changedDefs vm funcs' ++ funcs
  where funcs' = Map.keys $ Map.filter (not . null . List.intersect funcs . getFuncs []) (binds vm)

updateInline :: FuncName -> QVM -> QVM
updateInline fname vm = if isJust i_f
  then vm
  else inlineFunc fname to_inline_in_f $ foldr updateInline vm to_inline_in_f
  where i_f = (i_binds vm) Map.! fname
        f = (binds vm) Map.! fname
        to_inline_in_f = inlinableFuncs f $ binds vm

inlineFunc :: FuncName -> [FuncName] -> QVM -> QVM
inlineFunc fname to_inline vm = vm { i_binds = Map.insert fname (Just f_inlined) (i_binds vm) }
  where f_inlined = head $ inline ((binds vm) Map.! fname) to_inline vm

-- Note: No var renaming yet... (this will cause problems)
inline :: QItem -> [FuncName] -> QVM -> [QItem]
inline (QQuote pattern body) to_inline vm = [QQuote pattern $ Seq.fromList . concat $ inlined_body]
  where inlined_body = fmap (\x -> inline x to_inline vm) body
inline (QAtom a) to_inline vm = if elem a to_inline
  then let inline_expr = (\(Just x) -> x) $ (i_binds vm) Map.! a in [inline_expr, QAtom "call"]
  else [QAtom a]
inline x _ _ = [x]
