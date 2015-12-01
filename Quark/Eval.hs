{-# LANGUAGE ViewPatterns #-}

module Quark.Eval (eval, raiseError) where

import Quark.Type
import Quark.Parse
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import Data.Sequence ((><), (<|), (|>), viewr, viewl)
import Data.Sequence (ViewL(..))
import Data.Sequence (ViewR(..))
import Data.Maybe
import Data.List
import System.Process
import Control.Exception


--- Evaluation ---

-- main evaluation function, takes a quark vm and (assuming there aren't any errors) returns its reduction
-- if the top item is a fuction, run it
-- otherwise, push the top item to the data stack
eval :: QVM -> IState
eval vm = case viewl (prog vm) of
  ((QAtom f) :< sq) -> (getFunc (binds vm) f) $ vm { prog = sq }
  (x :< sq) -> return . Just $ vm { stack = x : (stack vm), prog = sq }

-- tries to retrieve a core or runtime defined function
-- if this fails, returns a `No such function: ` yeilding function
getFunc :: QLib -> FuncName -> QFunc
getFunc bindings fname = case coreFunc fname of
  Just f -> f
  Nothing -> case libFunc bindings fname of
    Just f -> f
    Nothing -> (\_ -> raiseError ("No such function: " ++ fname))

-- gets a runtime defined function, if no such function exists returns `Nothing`
libFunc :: QLib -> FuncName -> Maybe QFunc
libFunc bindings fname = Map.lookup fname bindings >>= (\f -> Just (\vm -> tryQuote f vm))

-- this is the function responsible for the behavior of the `call` quark function
tryQuote :: QItem -> QVM -> IState
tryQuote (QQuote p b) vm = case patternMatch p (stack vm) of
  Just bindings -> callQuote b bindings vm'
  Nothing -> return . Just $ pushVM (QSym "nil") vm'
  where vm' = dropVM (Seq.length p) vm
tryQuote _ _ = raiseError "Tried to call a value that wasn't a quote"

-- appends quote body to VM prog queue after subbing
callQuote :: QProg -> QLib -> QVM -> IState
callQuote prog vars vm = return . Just $ pushProgQVM vm $ fmap (qSub vars) prog

-- substitutes pattern terms
qSub :: QLib -> QItem -> QItem
qSub vars (QAtom a) = case Map.lookup a vars of { Just x -> x; Nothing -> QAtom a; }
qSub vars (QQuote p b) = QQuote (fmap (qSub vars) p) (fmap (qSub vars) b)
qSub _ x = x

-- checks to make sure the items a quote is being applied to match the quotes pattern
-- if these items do match, it returns the variable bindings for the quote body
patternMatch :: QProg -> QStack -> Maybe QLib
patternMatch pattern stack = qmatch Map.empty pattern stack
  where qmatch vars pattern stack = case (viewr pattern, stack) of
          (Seq.EmptyR, _) -> Just vars
          (_, []) -> Nothing
          ((sq :> (QAtom x)), (y : ys)) -> if Map.member x vars
            then if (vars Map.! x) == y then qmatch vars sq ys else Nothing
            else qmatch (Map.insert x y vars) sq ys
          ((sq :> x), (y : ys)) -> if x == y then qmatch vars sq ys else Nothing


--- Core Function Utils ---


-- core function dispatch
coreFunc :: FuncName -> Maybe QFunc
coreFunc = cf
  where
    -- numeric functions
    cf "+" = Just $ qNumFunc "+" (+)
    cf "*" = Just $ qNumFunc "*" (*)
    cf "/" = Just $ qNumFunc "/" (/)
    -- pure functions
    cf "<" = Just $ qTwoToOne "<" [Num, Num] qlessthan
    cf "<<" = Just $ qTwoToOne "<<" [Quote, Any] qpush
    cf ">>" = Just $ qOneToMulti ">>" [Quote] qpop
    cf "@+" = Just $ qTwoToOne "@+" [Quote, Quote] qunite
    cf "@-" = Just $ qOneToMulti "@-" [Quote] qseparate
    cf "show" = Just $ qOneToOne "show" [Any] qshow
    cf "chars" = Just $ qOneToOne "chars" [Str] qchars
    cf "weld" = Just $ qTwoToOne "weld" [Str, Str] qweld
    cf "type" = Just $ qOneToOne "type" [Any] qtypei
    cf "def" = Just $ qPureFunc "def" [Quote, Sym] qdef
    cf "parse" = Just $ qOneToMulti "parse" [Str] qparsei
    -- impure functions
    cf "call" = Just $ qFunc "call" [Quote] qcall
    cf "match" = Just $ qFunc "match" [Quote] qmatch
    cf "." = Just $ qFunc "." [] qprintstack
    cf "load" = Just $ qFunc "load" [Str] qload
    cf "write" = Just $ qFunc "write" [Str, Str] qwrite
    cf "cmd" = Just $ qFunc "cmd" [Str] qcmd
    cf "print" = Just $ qFunc "print" [Str] qprint
    cf "exit" = Just $ qFunc "exit" [] qexit
    cf x = Nothing


-- Error Builders

-- raises an error in the quark interpreter
raiseError :: String -> IState
raiseError str = putStrLn ("ERROR: " ++ str) >> return Nothing

-- specific error function for type mis-matches of quark core functions
raiseTypeError :: String -> QTypeSig -> QStack -> IState
raiseTypeError name sig stack = raiseError typeError
  where stack_sig = map qtype . reverse . take (length sig) $ stack
        typeError = "Primary function: "
                    ++ name
                    ++ " expected a stack of: "
                    ++ (show sig)
                    ++ " but instead got: "
                    ++ (show stack_sig)


-- Function Builders

-- wraps a quark function with type checking
qFunc :: FuncName -> QTypeSig -> QFunc -> QFunc
qFunc name sig f = (\vm -> if qtypeCheck (reverse sig) (stack vm)
  then f vm
  else raiseTypeError name sig (stack vm))

-- makes a pure quark function
qPureFunc :: FuncName -> QTypeSig -> (QVM -> QVM) -> QFunc
qPureFunc name sig f = qFunc name sig (return . Just . f)

-- makes a (QItem -> QItem) into a quark function
qOneToOne :: FuncName -> QTypeSig -> (QItem -> QItem) -> QFunc
qOneToOne name sig f = qPureFunc name sig f'
  where f' (QVM (x : xs) prog binds) = QVM ((f x) : xs) prog binds

-- makes a (QItem -> QItem -> QItem) into a quark function
qTwoToOne :: FuncName -> QTypeSig -> (QItem -> QItem -> QItem) -> QFunc
qTwoToOne name sig f = qPureFunc name sig f'
  where f' (QVM (x : y : xs) prog binds) = QVM (f x y : xs) prog binds

-- makes a (QItem -> [QItem]) into a quark function
qOneToMulti :: FuncName -> QTypeSig -> (QItem -> [QItem]) -> QFunc
qOneToMulti name sig f = qPureFunc name sig f'
  where f' (QVM (x : xs) prog binds) = QVM ((f x) ++ xs) prog binds

-- makes a numeric quark function
qNumFunc :: FuncName -> (Double -> Double -> Double) -> QFunc
qNumFunc name f = qPureFunc name [Num, Num] f'
  where f' (QVM (QNum a : QNum b : stack) prog binds) = QVM (QNum (f a b) : stack) prog binds


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
