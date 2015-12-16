module Quark.Serialize where

import Quark.Type
import qualified Data.Sequence as Seq
import Data.Foldable (toList)

--- Serialization ---

replaceChar :: Char -> String -> String -> String
replaceChar oldC newC str = concat $ map (\c -> if c == oldC then newC else [c]) str

-- converts a quark item into a string of the equivelent quark code
-- the number parameter `n` will shorten quotes to this length and add an ellipsis
-- if `n` is 0 no shortening will occur
serializeQ :: Int -> QItem -> String
serializeQ _ (QNum x) = if (ceiling x) == (floor x) then (show . floor) x else show x
serializeQ _ (QFunc x) = x
serializeQ _ (QVar x) = x
serializeQ _ (QSym x) = ':' : x
serializeQ _ (QStr x) = "\"" ++ escaped ++ "\""
  where escaped = replaceChar '"' "\\\"" $ replaceChar '\\' "\\\\" x
serializeQ n (QQuote pattern body) = "[ " ++ patternStr ++ bodyStr ++ " ]"
  where getItems = if n == 0 then id else take n
        ellipsis xs = if (Seq.length xs > n) && (n /= 0) then " ..." else ""
        quoteSeqToStr = unwords . map (serializeQ n) . getItems . toList
        patternStr = if Seq.null pattern then "" else (quoteSeqToStr pattern) ++ (ellipsis pattern) ++ " | "
        bodyStr = (quoteSeqToStr body) ++ (ellipsis body)
