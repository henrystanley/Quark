{-# LANGUAGE ViewPatterns #-}

module Quark.Optimize (optimize) where

import Quark.Type
import qualified Data.Sequence as Seq
import Data.Sequence (viewl, (><), (<|))
import Data.Sequence (ViewL(..))

-- reduces a quark item to a less complex but semantically equivalent version
optimize :: QItem -> QItem
optimize = recQProgOpt redundantCall

recQProgOpt :: (QProg -> QProg) -> (QItem -> QItem)
recQProgOpt opt = rec_opt
  where rec_opt (QQuote p b) = QQuote p $ opt $ fmap rec_opt b
        rec_opt x = x

redundantCall :: QProg -> QProg
redundantCall (viewl -> (QQuote (viewl -> EmptyL) b) :< (viewl -> (QFunc "call") :< rest)) = b >< (redundantCall rest)
redundantCall (viewl -> x :< rest) = x <| (redundantCall rest)
redundantCall (viewl -> EmptyL) = Seq.empty
