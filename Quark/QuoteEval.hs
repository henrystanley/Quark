module Quark.QuoteEval where

import Quark.Type
import Quark.QVM
import Quark.Errors
import Data.Sequence (viewr)
import Data.Sequence (ViewR(..))
import qualified Data.Sequence as Seq
import qualified Data.Map.Strict as Map


--- Pattern Matching & Quote Calling --

-- this is the function responsible for the behavior of the `call` quark function
tryQuote :: QItem -> QVM -> IState
tryQuote (QQuote p b) vm = case patternMatch p (stack vm) of
  Just bindings -> callQuote b bindings vm'
  Nothing -> return . Just $ pushVM (QSym "nil") vm'
  where vm' = dropVM (Seq.length p) vm
tryQuote _ vm = raiseError "Tried to call a value that wasn't a quote" vm

-- used to call a function quote, essentially the same as tryQuote but with callstack push and pop
callFunc :: FuncName -> QItem -> QVM -> IState
callFunc fname (QQuote p b) vm = case patternMatch p (stack vm) of
  Just bindings -> callQuote b bindings vm'
  Nothing -> return . Just $ pushVM (QSym "nil") vm'
  where vm' = pushCallVM fname $ pushProgVM (Seq.singleton $ QMagic PopCallStack) $ dropVM (Seq.length p) vm
callFunc _ _ vm = raiseError "Tried to call a function that wasn't a quote" vm

-- appends quote body to VM prog queue after subbing
callQuote :: QProg -> QLib -> QVM -> IState
callQuote prog vars = return . Just . pushProgVM (fmap (qSub vars) prog)

-- substitutes pattern terms
qSub :: QLib -> QItem -> QItem
qSub vars (QVar a) = case Map.lookup a vars of { Just x -> x; Nothing -> QVar a; }
qSub vars (QQuote p b) = QQuote (fmap (qSub vars) p) (fmap (qSub vars) b)
qSub _ x = x

-- checks to make sure the items a quote is being applied to match the quotes pattern
-- if these items do match, it returns the variable bindings for the quote body
patternMatch :: QProg -> QStack -> Maybe QLib
patternMatch pattern stack = qmatch Map.empty pattern stack
  where qmatch vars pattern stack = case (viewr pattern, stack) of
          (Seq.EmptyR, _) -> Just vars
          (_, []) -> Nothing
          ((sq :> (QVar x)), (y : ys)) -> if Map.member x vars
            then if (vars Map.! x) == y then qmatch vars sq ys else Nothing
            else qmatch (Map.insert x y vars) sq ys
          ((sq :> x), (y : ys)) -> if x == y then qmatch vars sq ys else Nothing
