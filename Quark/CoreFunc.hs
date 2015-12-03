{-# LANGUAGE ViewPatterns #-}

module Quark.CoreFunc (coreFunc) where

import Quark.Type
import Quark.QType
import Quark.Parse
import Quark.Serialize
import Quark.CoreFuncUtils
import Quark.QVM
import Quark.Errors
import Quark.QuoteEval
import Data.List
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import Data.Sequence ((><), (<|), (|>), viewr, viewl)
import Data.Sequence (ViewL(..))
import Data.Sequence (ViewR(..))
import System.Process
import Control.Exception

-- Core Function Dispatch

-- map of function names to core functions
coreFunc :: Map.Map FuncName QFunc
coreFunc = Map.fromList [ "+" #=> qNumFunc (+)
                        , "*" #=> qNumFunc (*)
                        , "/" #=> qNumFunc (/)
                        , "<" #=> qTwoToOne [Num, Num] qlessthan
                        , "<<" #=> qTwoToOne [Quote, Any] qpush
                        , ">>" #=> qOneToMulti [Quote] qpop
                        , "@+" #=> qTwoToOne [Quote, Quote] qunite
                        , "@-" #=> qOneToMulti [Quote] qseparate
                        , "show" #=> qOneToOne [Any] qshow
                        , "chars" #=> qOneToOne [Str] qchars
                        , "weld" #=> qTwoToOne [Str, Str] qweld
                        , "type" #=> qOneToOne [Any] qtypei
                        , "def" #=> qPureFunc [Quote, Sym] qdef
                        , "parse" #=> qOneToMulti [Str] qparsei
                        , "call" #=> qFunc [Quote] qcall
                        , "match" #=> qFunc [Quote] qmatch
                        , "." #=> qFunc [] qprintstack
                        , "load" #=> qFunc [Str] qload
                        , "write" #=> qFunc [Str, Str] qwrite
                        , "cmd" #=> qFunc [Str] qcmd
                        , "print" #=> qFunc [Str] qprint
                        , "exit" #=> qFunc [] qexit ]


--- Core Function Implementations ---


-- Pure Functions:

-- compares two numbers
qlessthan (QNum x) (QNum y) = QSym $ if x < y then "true" else "false"

-- pushes an item into a quote body
qpush x (QQuote a sq) = QQuote a (sq |> x)

-- pops an item from a quote body
qpop (QQuote a (viewr -> sq :> x)) = [x, (QQuote a sq)]
qpop x = [x]

-- makes the body of the second quote the pattern of the first quote
qunite (QQuote _ xs) (QQuote _ ys) = QQuote ys xs

-- splits a quote into two new quotes, whose bodies contain the pattern and body of the original quote
qseparate (QQuote ys xs) = [(QQuote Seq.empty xs), (QQuote Seq.empty ys)]

-- pops an item, and pushes the type of this item as a symbol
qtypei = qtypeLiteral . qtype

-- pops an item and pushes its string representation using serializeQ
qshow = QStr . serializeQ 0

-- pops a string and pushes a quote containing a string for each character in the string
qchars (QStr xs) = QQuote Seq.empty strChars
  where strChars = Seq.fromList $ map (QStr . (\c -> [c])) xs

-- pops two strings and concats them
qweld (QStr a) (QStr b) = QStr $ b ++ a

-- pops a symbol and quote. binds the symbol to the quote as a function in the vm
qdef vm = vm { stack = stack', binds = Map.insert fname f (binds vm) }
  where ((QSym fname) : f : stack') = stack vm

-- parses a string containing quark code
qparsei (QStr x) = case qParse x of
  Left _ -> [QSym "not-ok"]
  Right qvals -> [QSym "ok", parsedQuote]
    where parsedQuote = QQuote Seq.empty (Seq.fromList qvals)


-- Scary Impure Functions:

-- calls a quote
qcall (QVM (quote : stack) prog binds) = tryQuote quote $ QVM stack prog binds

-- calls the first quote in a list of quotes that has a matching pattern
qmatch vm = (tryQuotes quotes) vm { stack = stack' }
  where ((QQuote _ quotes) : stack') = stack vm
        tryQuotes qs = case viewl qs of
          Seq.EmptyL -> return . Just
          ((QQuote p b) :< sq) -> case patternMatch p stack' of
            Just bindings -> callQuote b bindings . dropVM (Seq.length p)
            Nothing -> tryQuotes sq
          (_ :< _) -> (\_ -> raiseError "Non quote value found in `match` call")

-- pops a string and prints it without a linebreak
qprint (QVM ((QStr x) : stack) prog binds) = putStr x >> (return . Just $ QVM stack prog binds)

-- prints the contents of the entire stack with a linebreak
qprintstack vm = (putStrLn . intercalate " " . map (serializeQ 0) . reverse . stack) vm >> (return . Just $ vm)

-- pops a string and loads the file with this filename, then pushes back the contents of the file as a string
qload (QVM ((QStr filename) : stack) prog binds) = do
  read_str <- try (readFile filename) :: IO (Either SomeException String)
  let stack_top = case read_str of {
    Left _ -> [QSym "not-ok"];
    Right s -> [QSym "ok", QStr s]; }
  return . Just $ QVM (stack_top ++ stack) prog binds

-- pops two strings, uses the first as a filename to save as and the second as the file contents
qwrite (QVM ((QStr filename) : (QStr toWrite) : stack) prog binds) = do
  wrote <- try (writeFile filename toWrite) :: IO (Either SomeException ())
  let stack_top = case wrote of {
    Left _ -> [QSym "not-ok"];
    Right _ -> [QSym "ok"]; }
  return . Just $ QVM (stack_top ++ stack) prog binds

-- pops a string and runs it as a shell command, pushes the output of the command as a string
qcmd (QVM ((QStr cmd) : stack) prog binds) = do
  result <- try (System.Process.readCreateProcess (System.Process.shell cmd) "") :: IO (Either SomeException String)
  let stack_top = case result of {
    Left _ -> [QSym "not-ok"];
    Right s -> [QSym "ok", QStr s]; }
  return . Just $ QVM (stack_top ++ stack) prog binds

-- exits the interpreter (only if in script mode)
qexit _ = return Nothing
