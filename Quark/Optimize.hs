{-# LANGUAGE ViewPatterns #-}

module Quark.Optimize (optimize) where

import Quark.Type
import qualified Data.Sequence as Seq
import Data.Sequence (viewl, viewr, (><), (<|))
import Data.Sequence (ViewL(..))
import Data.Sequence (ViewR(..))

--- Utils ---

recQProgOpt :: (QProg -> QProg) -> (QItem -> QItem)
recQProgOpt opt = rec_opt
  where rec_opt (QQuote p b) = QQuote p $ opt $ fmap rec_opt b
        rec_opt x = x

partialApplySub :: (QItem, QItem) -> QItem -> QItem
partialApplySub (from, to) (QQuote p b) = QQuote (fmap no_rec_sub p) (fmap (partialApplySub (from, to)) b)
  where no_rec_sub x = if x == from then to else x
partialApplySub (from, to) x = if x == from then to else x


--- Optimization ---

redundantCall_O :: QProg -> QProg
redundantCall_O (viewl -> (QQuote (viewl -> EmptyL) b) :< (viewl -> (QFunc "call") :< rest)) = b >< (redundantCall_O rest)
redundantCall_O (viewl -> x :< rest) = x <| (redundantCall_O rest)
redundantCall_O (viewl -> EmptyL) = Seq.empty

partialApply_O :: QProg -> QProg
partialApply_O (viewl -> x :< (viewl -> z :< (viewl -> (QFunc "call") :< rest))) = case z of
  QQuote (viewr -> rest_pattern :> (QVar v)) b -> case x of
    QFunc _ -> no_opt
    _ -> (partialApplySub (QVar v, x) (QQuote rest_pattern b)) <| (QFunc "call") <| (partialApply_O rest)
  QQuote (viewr -> rest_pattern :> v) b -> if v == x
    then (QQuote rest_pattern b) <| (QFunc "call") <| (partialApply_O rest)
    else no_opt
  _ -> no_opt
  where no_opt = x <| z <| (QFunc "call") <| (partialApply_O rest)
partialApply_O (viewl -> x :< rest) = x <| (partialApply_O rest)
partialApply_O (viewl -> EmptyL) = Seq.empty


-- reduces a quark item to a less complex but semantically equivalent version
optimize :: QItem -> QItem
optimize x = if x == x' then x else optimize x'
  where opts = recQProgOpt partialApply_O . recQProgOpt redundantCall_O
        x' = opts x
