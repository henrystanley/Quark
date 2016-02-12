module Quark.CoreFunc (coreFunc) where

import Quark.Type
import Quark.QType
import Quark.Parse
import Quark.Serialize
import Quark.QVM
import Quark.Errors
import Quark.QuoteEval
import Quark.Inline
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified System.Process as SysProc
import Data.Sequence ((><), (<|), (|>), viewr, viewl)
import Data.Sequence (ViewL(..))
import Data.Sequence (ViewR(..))
import Control.Exception


-- Core Function Dispatch ---

-- applies a core function
coreFunc :: FuncName -> QVM -> IState
coreFunc coreFunc = cf
  where cf = case coreFunc of
          "+" -> qAdd
          "*" -> qMul
          "/" -> qDiv
          "<" -> qLessThan
          "@>" -> qPopFront
          "@<" -> qPushFront
          "<@" -> qPopBack
          ">@" -> qPushBack
          "><" -> qUnite
          "<>"-> qSeparate
          "show" -> qShow
          "chars" -> qChars
          "weld" -> qWeld
          "type" -> qTypeOf
          "def" -> qDef
          "parse" -> qParseStr
          "call" -> qCall
          "match" -> qMatch
          "." -> qPrintStack
          "load" -> qLoad
          "write" -> qWrite
          "cmd" -> qCmd
          "print" -> qPrint
          "exit" -> qExit
          _ -> raiseError ("No such function: " ++ coreFunc)


--- Core Function Implementations ---

-- addition
qAdd :: QVM -> IState
qAdd vm = case stack vm of
  (QNum x : QNum y : stack') -> return . Just $ vm { stack = QNum (x + y) : stack' }
  _ -> raiseTypeError "+" [Num, Num] vm

-- multiplication
qMul :: QVM -> IState
qMul vm = case stack vm of
  (QNum x : QNum y : stack') -> return . Just $ vm { stack = QNum (x * y) : stack' }
  _ -> raiseTypeError "*" [Num, Num] vm

-- division
qDiv :: QVM -> IState
qDiv vm = case stack vm of
  (QNum x : QNum 0 : stack') -> raiseError "Tried to divide by zero" vm
  (QNum x : QNum y : stack') -> return . Just $ vm { stack = QNum (x / y) : stack' }
  _ -> raiseTypeError "/" [Num, Num] vm

-- compares two numbers
qLessThan :: QVM -> IState
qLessThan vm = case stack vm of
  (QNum x : QNum y : stack') -> return . Just $ vm { stack = QSym comparison : stack' }
    where comparison = if x < y then "true" else "false"
  _ -> raiseTypeError "<" [Num, Num] vm

-- appends an item to the front of a quote body
qPushFront :: QVM -> IState
qPushFront vm = case stack vm of
  (x : QQuote a sq : stack') -> return . Just $ vm { stack = QQuote a (sq |> x) : stack' }
  _ -> raiseTypeError "@<" [Quote, Any] vm

-- pops an item from the front of a quote body
qPopFront :: QVM -> IState
qPopFront vm = case stack vm of
  (QQuote a sq : stack') -> return . Just $ vm { stack = popItem : QQuote a sq' : stack' }
    where (popItem, sq') = case viewr sq of
            (rest :> x) -> (x, rest)
            _ -> (QSym "nil", sq)
  _ -> raiseTypeError "@>" [Quote] vm

-- appends an item to the end of a quote body
qPushBack :: QVM -> IState
qPushBack vm = case stack vm of
  (QQuote a sq : x : stack') -> return . Just $ vm { stack = QQuote a (x <| sq) : stack' }
  _ -> raiseTypeError ">@" [Any, Quote] vm

-- pops an item from the end of a quote body
qPopBack :: QVM -> IState
qPopBack vm = case stack vm of
  (QQuote a sq : stack') -> return . Just $ vm { stack = QQuote a sq' : popItem : stack' }
    where (popItem, sq') = case viewl sq of
            (x :< rest) -> (x, rest)
            _ -> (QSym "nil", sq)
  _ -> raiseTypeError "<@" [Quote] vm

-- makes the body of the second quote the pattern of the first quote
qUnite :: QVM -> IState
qUnite vm = case stack vm of
  (QQuote a1 sq1 : QQuote a2 sq2 : stack') -> return . Just $ vm'
    where vm' = vm { stack = QQuote sq2 sq1 : stack' }
  _ -> raiseTypeError "><" [Quote, Quote] vm

-- splits a quote into two new quotes
-- the bodies of the new quotes contain the pattern and body of the original quote
qSeparate :: QVM -> IState
qSeparate vm = case stack vm of
  (QQuote a sq : stack') -> return . Just $ vm'
    where bodyQuote = QQuote Seq.empty sq
          patternQuote = QQuote Seq.empty a
          vm' = vm { stack = bodyQuote : patternQuote : stack' }
  _ -> raiseTypeError "<>" [Quote] vm

-- pops an item, and pushes the type of this item as a symbol
qTypeOf :: QVM -> IState
qTypeOf vm = case stack vm of
  (x : stack') -> return . Just $ vm { stack = (qtypeLiteral . qtype) x : stack' }
  _ -> raiseTypeError "type" [Any] vm

-- pops an item and pushes its string representation using serializeQ
qShow :: QVM -> IState
qShow vm = case stack vm of
  (x : stack') -> return . Just $ vm { stack = (QStr . serializeQ 0) x : stack' }
  _ -> raiseTypeError "show" [Any] vm

-- pops a string and pushes a quote containing a string for each character in the string
qChars :: QVM -> IState
qChars vm = case stack vm of
  (QStr str : stack') -> return . Just $ vm { stack = QQuote Seq.empty strChars : stack' }
    where strChars = Seq.fromList $ map (QStr . (\c -> [c])) str
  _ -> raiseTypeError "chars" [Str] vm

-- pops two strings and concats them
qWeld :: QVM -> IState
qWeld vm = case stack vm of
  (QStr str1 : QStr str2 : stack') -> return . Just $ vm'
    where vm' = vm { stack = QStr (str2 ++ str1) : stack' }
  _ -> raiseTypeError "weld" [Str, Str] vm

-- pops a symbol and quote. binds the symbol to the quote as a function in the vm
qDef :: QVM -> IState
qDef vm = case stack vm of
  (QSym fname : QQuote a sq : stack') -> return . Just $ vm'
    where binds' = Map.insert fname (QQuote a sq) (binds vm)
          vm' = vm { stack = stack', binds = binds' }
  _ -> raiseTypeError "def" [Quote, Sym] vm

-- parses a string containing quark code
qParseStr :: QVM -> IState
qParseStr vm = case stack vm of
  (QStr str : stack') -> return . Just $ vm { stack = parsed ++ stack' }
    where parsed = case qParse str of
            Left _ -> [QSym "not-ok"]
            Right qvals -> [QSym "ok", QQuote Seq.empty $ Seq.fromList qvals]
  _ -> raiseTypeError "parse" [Str] vm

-- calls a quote
qCall :: QVM -> IState
qCall vm = case stack vm of
  (QQuote a sq : stack') -> tryQuote (QQuote a sq) $ vm { stack = stack' }
  _ -> raiseTypeError "call" [Quote] vm

-- calls the first quote in a list of quotes that has a matching pattern
qMatch :: QVM -> IState
qMatch vm = case stack vm of
  (QQuote a sq : stack') -> tryQuotes sq $ vm { stack = stack' }
    where tryQuotes quotes = case viewl quotes of
            Seq.EmptyL -> return . Just
            (QQuote pattern body :< quotes') -> case patternMatch pattern stack' of
              Just bindings -> callQuote body bindings . dropVM (Seq.length pattern)
              Nothing -> tryQuotes quotes'
            (_ :< _) -> raiseError "Non quote value found in `match` call"
  _ -> raiseTypeError "match" [Quote] vm

-- pops a string and prints it without a linebreak
qPrint :: QVM -> IState
qPrint vm = case stack vm of
  (QStr str : stack') -> putStr str >> (return . Just $ vm { stack = stack' })
  _ -> raiseTypeError "print" [Str] vm

-- prints the contents of the entire stack with a linebreak
qPrintStack :: QVM -> IState
qPrintStack vm = do
  putStrLn . unwords . map (serializeQ 0) . reverse . stack $ vm
  return . Just $ vm

-- pops a string and loads the file with this filename
-- pushes the contents of the file as a string back on the stack
qLoad :: QVM -> IState
qLoad vm = case stack vm of
  (QStr filename : stack') -> do
    fileContents <- try $ readFile filename :: IO (Either SomeException String)
    let stackTop = case fileContents of
          Left _ -> [QSym "not-ok"]
          Right contents -> [QSym "ok", QStr contents]
    return . Just $ vm { stack = stackTop ++ stack' }
  _ -> raiseTypeError "load" [Str] vm

-- pops two strings: a filename to save as, and the file contents to write
qWrite :: QVM -> IState
qWrite vm = case stack vm of
  (QStr filename : QStr toWrite : stack') -> do
    wrote <- try $ writeFile filename toWrite :: IO (Either SomeException ())
    let stackTop = case wrote of
          Left _ -> [QSym "not-ok"]
          Right _ -> [QSym "ok"]
    return . Just $ vm { stack = stackTop ++ stack' }
  _ -> raiseTypeError "write" [Str, Str] vm

-- pops a string and runs it as a shell command, pushes the output of the command
qCmd :: QVM -> IState
qCmd vm = case stack vm of
  (QStr cmd : stack') -> do
    let proc = SysProc.readCreateProcess (SysProc.shell cmd) ""
    cmdResult <- try $ proc :: IO (Either SomeException String)
    let stackTop = case cmdResult of
          Left _ -> [QSym "not-ok"]
          Right result -> [QSym "ok", QStr result]
    return . Just $ vm { stack = stackTop ++ stack' }
  _ ->  raiseTypeError "cmd" [Str] vm

-- exits the interpreter (only if in script mode)
qExit :: QVM -> IState
qExit _ = return Nothing
